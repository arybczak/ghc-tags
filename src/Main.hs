module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Foldable qualified as F
import Data.Function
import Data.Functor
import Data.IORef
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Primitive.Array
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import GHC (GhcException, setSessionDynFlags)
import GHC.Conc (getNumProcessors)
import GHC.Data.Bag
import GHC.Data.StringBuffer
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Env.Types
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Hs
import GHC.Parser.Errors.Types
import GHC.Parser.Lexer
import GHC.Types.Error
import GHC.Types.SrcLoc
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Process
import Yamlet qualified as Y

import GhcTags
import GhcTags.CTag qualified as CTag
import GhcTags.CTag.Header
import GhcTags.Config.Args
import GhcTags.Config.Project
import GhcTags.ETag qualified as ETag
import GhcTags.GhcCompat

----------------------------------------

data Updated a = Updated Bool a

data HsFileType = HsFile | HsBootFile | LHsFile | AlexFile | HscFile

data WorkerData = WorkerData
  { wdTags :: MVar DirtyTags
  , wdTimes :: MVar DirtyModTimes
  , wdQueue :: TBQueue (Maybe (FilePath, HsFileType, UTCTime))
  , wdOutputLock :: MVar ()
  , wdFailed :: IORef Bool
  }

generateTagsForProject :: Int -> WorkerData -> ProjectConfig -> IO ()
generateTagsForProject threads wd pc =
  runConcurrently . F.fold $
    Concurrently (processFiles (pcSourcePaths pc) >> terminateWorkers)
      : replicate threads (Concurrently worker)
  where
    -- Both sides of the exclusion test go through the same normalisation, so
    -- that "./dist", "dist/" and "dist" mean the same directory.
    normalisePath :: FilePath -> FilePath
    normalisePath = dropTrailingPathSeparator . normalise

    excludePaths :: Set.Set FilePath
    excludePaths = Set.fromList . map normalisePath $ pcExcludePaths pc

    -- Walk a list of paths recursively and process eligible source files.
    processFiles :: [String] -> IO ()
    processFiles = mapM_ $ \origPath -> do
      let path = normalisePath origPath
      unless (path `Set.member` excludePaths) $ do
        doesDirectoryExist path >>= \case
          True -> do
            paths <- map (path </>) <$> listDirectory path
            processFiles paths
          False -> F.forM_ (takeExtension path `lookup` haskellExts) $ \hsType -> do
            showIOError $ do
              -- Source files are scanned and updated only if their mtime changed or
              -- it's not recorded.
              time <- getModificationTime path
              updateTags <- withMVar (wdTimes wd) $ \times -> pure $
                case TagFileName (T.pack path) `Map.lookup` times of
                  -- If the file was already updated, it means it's eligible for
                  -- the update with regard to its mtime, but it was already
                  -- processed. In such case we let it through in order to
                  -- support the case of parsing the same file multiple times
                  -- with different CPP options.
                  Just (Updated updated oldTime) -> updated || oldTime < time
                  Nothing -> True
              when updateTags $ do
                atomically . writeTBQueue (wdQueue wd) $ Just (path, hsType, time)
      where
        haskellExts =
          [ (".hs", HsFile)
          , (".hs-boot", HsBootFile)
          , (".lhs", LHsFile)
          , (".x", AlexFile)
          , (".hsc", HscFile)
          ]

    terminateWorkers :: IO ()
    terminateWorkers = atomically $ do
      replicateM_ threads $ writeTBQueue (wdQueue wd) Nothing

    showIOError m =
      m `catch` \(e :: IOError) -> do
        putError $ "Error: " ++ show e

    -- Several threads report at once, and stderr is unbuffered, so without
    -- the lock their messages mix character by character.
    putMessage :: String -> IO ()
    putMessage msg = withMVar (wdOutputLock wd) $ \() -> hPutStrLn stderr msg

    putError :: String -> IO ()
    putError msg = markFailed >> putMessage msg

    markFailed :: IO ()
    markFailed = atomicWriteIORef (wdFailed wd) True

    -- Extract tags from a given file and update the TagMap.
    worker :: IO ()
    worker = runGhc $ do
      void $ setSessionDynFlags . adjustDynFlags pc =<< getSessionDynFlags
      -- GHC prints the errors of the C preprocessor itself.
      pushLogHookM $ \logAction flags msgClass srcSpan msg ->
        withMVar (wdOutputLock wd) $ \() -> logAction flags msgClass srcSpan msg
      env <- getSession
      liftIO . fix $ \loop ->
        atomically (readTBQueue $ wdQueue wd) >>= \case
          Nothing -> pure ()
          Just (file, hsType, mtime) -> do
            showIOError $ processFile env file hsType mtime
            loop
      where
        processFile :: HscEnv -> FilePath -> HsFileType -> UTCTime -> IO ()
        processFile env rawFile hsType mtime = withHsFile rawFile hsType $ \hsFile -> do
          handle showErr $
            preprocessFile hsFile >>= \case
              Left errs -> do
                report (hsc_dflags env) errs
                markFailed
              Right (flags, file) -> do
                -- when (file /= rawFile) $ do
                --  putStrLn $ "Processing " ++ file ++ " (" ++ rawFile ++ ")"
                buffer <- hGetStringBuffer file
                case parseModule file flags buffer of
                  PFailed pstate -> do
                    let (wrns, errs) = getPsMessages pstate
                    report flags wrns
                    report flags errs
                    markFailed
                  POk pstate hsModule -> do
                    let (wrns, errs) = getPsMessages pstate
                    report flags wrns
                    report flags errs
                    if isEmptyBag $ getMessages errs
                      then do
                        modifyMVar_ (wdTags wd) $ \tags -> do
                          pure $! updateTagsWith flags hsModule tags
                        modifyMVar_ (wdTimes wd) $ \times -> do
                          let path = TagFileName $ T.pack rawFile
                          pure $! updateTimesWith path mtime times
                      else markFailed
          where
            showErr :: GHC.GhcException -> IO ()
            showErr = putError . show

            report :: forall e. Diagnostic e => DynFlags -> Messages e -> IO ()
            report flags =
              printMessages
                (hsc_logger env)
                (defaultDiagnosticOpts @e)
                (initDiagOpts flags)

            -- GHC rejects a file when an OPTIONS_GHC pragma contains a flag
            -- that the linked ghc library doesn't know, e.g. when the source
            -- tree is built with a newer compiler. Such flags don't influence
            -- tag generation, so blank them out and preprocess the file again.
            preprocessFile
              :: FilePath
              -> IO (Either DriverMessages (DynFlags, FilePath))
            preprocessFile file =
              preprocess env file Nothing Nothing >>= \case
                Left errs
                  | flagSpans@(_ : _) <- unknownFlagSpans errs -> do
                      content <- T.decodeUtf8Lenient <$> BS.readFile file
                      let buffer = stringToStringBuffer . T.unpack $ blankSpans flagSpans content
                      preprocess env file (Just buffer) Nothing
                result -> pure result

            unknownFlagSpans :: DriverMessages -> [RealSrcSpan]
            unknownFlagSpans errs = flip mapMaybe (bagToList $ getMessages errs) $ \msg ->
              case (errMsgDiagnostic msg, errMsgSpan msg) of
                (DriverPsHeaderMessage (PsHeaderMessage PsErrUnknownOptionsPragma {}), RealSrcSpan s _)
                  | srcSpanStartLine s == srcSpanEndLine s -> Just s
                _ -> Nothing

            -- Overwrite the spans with spaces so that the line and column
            -- numbers of the rest of the file stay the same.
            blankSpans :: [RealSrcSpan] -> T.Text -> T.Text
            blankSpans flagSpans = T.unlines . zipWith blankLine [1 ..] . T.lines
              where
                blankLine :: Int -> T.Text -> T.Text
                blankLine lineNo line =
                  foldl' blank line $ filter ((== lineNo) . srcSpanStartLine) flagSpans

                -- Leave the line alone if the span doesn't point at a flag, as
                -- then the column numbers don't match the decoded content.
                blank :: T.Text -> RealSrcSpan -> T.Text
                blank line s
                  | "-" `T.isPrefixOf` flag = before <> T.replicate (T.length flag) " " <> after
                  | otherwise = line
                  where
                    (before, rest) = T.splitAt (srcSpanStartCol s - 1) line
                    (flag, after) = T.splitAt (srcSpanEndCol s - srcSpanStartCol s) rest

        -- Alex and Hsc files need to be preprocessed before going into GHC.
        withHsFile :: FilePath -> HsFileType -> (FilePath -> IO ()) -> IO ()
        withHsFile file hsType k = case hsType of
          AlexFile -> preprocessWith "alex" []
          HscFile ->
            preprocessWith "hsc2hs" $
              map ("-I" ++) (pcCppIncludes pc)
                ++ filter ("-D" `isPrefixOf`) (pcCppOptions pc)
          _ -> k file
          where
            preprocessWith :: FilePath -> [FilePath] -> IO ()
            preprocessWith prog args = withSystemTempDirectory "ghc-tags" $ \dir -> do
              let tmpFile = dir </> "out.hs"
              (ec, out, err) <-
                readProcessWithExitCode
                  prog
                  ([file, "-o", tmpFile] ++ args)
                  ""
              case ec of
                ExitSuccess -> k tmpFile
                ExitFailure code ->
                  putError . intercalate "\n" $
                    [ "Preprocessing "
                        ++ file
                        ++ " with "
                        ++ prog
                        ++ " failed with exit code "
                        ++ show code
                    ]
                      ++ ["* STDOUT: " ++ out | not (null out)]
                      ++ ["* STDERR: " ++ err | not (null err)]

main :: IO ()
main = do
  -- The default number of threads is half the number of CPU cores as usually
  -- the other half are logical cores that don't increase performance when
  -- loaded (or even decrease it in case of high core count, e.g. Ryzen 5950x).
  defaultThreads <- max 1 . (`div` 2) <$> getNumProcessors

  args <- parseArgs defaultThreads =<< getArgs

  pcs <- case aSourcePaths args of
    SourceArgs paths -> pure [defaultProjectConfig {pcSourcePaths = paths}]
    ConfigFile configFile ->
      getProjectConfigs configFile >>= \case
        Just pcs -> pure pcs
        Nothing -> exitFailure

  when (not $ null pcs) $ do
    wd <- initWorkerData args (aThreads args)

    setNumCapabilities (aThreads args)
    forM_ pcs $ generateTagsForProject (aThreads args) wd
    setNumCapabilities 1

    cleanTagMap <- withMVar (wdTags wd) (cleanupTags args)
    writeTags (aTagFile args) cleanTagMap
    withMVar (wdTimes wd) $ Y.encodeFile (timesFile args) <=< cleanupTimes cleanTagMap

    failed <- readIORef (wdFailed wd)
    when failed exitFailure
  where
    timesFile args = aTagFile args <.> "mtime"

    initWorkerData :: Args -> Int -> IO WorkerData
    initWorkerData args threads = do
      tags@DirtyTags {dtTags} <- case aTagType args of
        ETag -> readTags SingETag (aTagFile args)
        CTag -> readTags SingCTag (aTagFile args)
      -- If tags are empty there is no point looking at mtimes.
      mtimes <-
        if Map.null dtTags
          then pure Map.empty
          else readTimes (timesFile args)
      wdTags <- newMVar tags
      wdTimes <- newMVar mtimes
      wdQueue <- newTBQueueIO (fromIntegral threads)
      wdOutputLock <- newMVar ()
      wdFailed <- newIORef False
      pure WorkerData {..}

----------------------------------------

type DirtyModTimes = Map.Map TagFileName (Updated UTCTime)
type ModTimes = Map.Map TagFileName UTCTime

-- | Read the file with mtimes of previously processed source files.
readTimes :: FilePath -> IO DirtyModTimes
readTimes timesFile =
  tryIOError (Y.decodeFile @ModTimes timesFile) <&> \case
    Right (Right times) -> Updated False <$> times
    _ -> Map.empty

-- | Update an mtime of a source file with a new value.
updateTimesWith :: TagFileName -> UTCTime -> DirtyModTimes -> DirtyModTimes
updateTimesWith file time = Map.insert file (Updated True time)

-- | Check if files that were not updated exist and drop them if they don't.
cleanupTimes :: Tags -> DirtyModTimes -> IO ModTimes
cleanupTimes Tags {..} = Map.traverseMaybeWithKey $ \file -> \case
  Updated updated time
    | updated || file `Map.member` tTags -> pure $ Just time
    | otherwise -> do
        let path = T.unpack $ getTagFileName file
        doesFileExist path >>= \case
          True -> pure $ Just time
          False -> pure Nothing

----------------------------------------

data DirtyTags = forall tt. DirtyTags
  { dtKind :: SingTagType tt
  , dtHeaders :: [CTag.Header]
  , dtTags :: Map.Map TagFileName (Updated (Set.Set (Tag tt)))
  }

data Tags = forall tt. Tags
  { tKind :: SingTagType tt
  , tHeaders :: [CTag.Header]
  , tTags :: Map.Map TagFileName [Tag tt]
  }

-- | Like 'try', but let asynchronous exceptions through. A tags file that
-- cannot be read must not stop the run, whatever the reason is.
trySync :: IO a -> IO (Either SomeException a)
trySync m =
  try m >>= \case
    Right a -> pure $ Right a
    Left err -> case fromException err of
      Just (SomeAsyncException _) -> throwIO err
      Nothing -> pure $ Left err

readTags :: forall tt. SingTagType tt -> FilePath -> IO DirtyTags
readTags tt tagsFile =
  doesFileExist tagsFile >>= \case
    False -> pure newDirtyTags
    True -> do
      res <- trySync $ do
        parsed <- parseTagsFile . T.decodeUtf8Lenient =<< BS.readFile tagsFile
        -- Full evaluation decreases performance variation. It also keeps a
        -- failure of the parser inside 'trySync'.
        evaluate $ force parsed
      case res of
        Right (Right (headers, tags)) ->
          pure
            DirtyTags
              { dtKind = tt
              , dtHeaders = headers
              , dtTags = Map.map (Updated False . Set.fromList) tags
              }
        -- reading failed
        Left err -> do
          hPutStrLn stderr $ "Error while reading " ++ tagsFile ++ ": " ++ show err
          pure newDirtyTags
        -- parsing failed
        Right (Left err) -> do
          hPutStrLn stderr $ "Error while parsing " ++ tagsFile ++ ": " ++ show err
          pure newDirtyTags
  where
    newDirtyTags =
      DirtyTags
        { dtKind = tt
        , dtHeaders = []
        , dtTags = Map.empty
        }

    parseTagsFile
      :: T.Text
      -> IO (Either String ([CTag.Header], Map.Map TagFileName [Tag tt]))
    parseTagsFile = case tt of
      SingETag -> fmap (fmap ([],)) . ETag.parseTagsFile
      SingCTag -> CTag.parseTagsFile

updateTagsWith :: DynFlags -> Located (HsModule GhcPs) -> DirtyTags -> DirtyTags
updateTagsWith dflags hsModule DirtyTags {..} =
  DirtyTags
    { dtTags = Map.unionWith mergeTags fileTags dtTags
    , ..
    }
  where
    mergeTags (Updated newUpdated newTags) (Updated oldUpdated oldTags) =
      -- If the file was already updated, we merge tags. This supports the case
      -- of parsing the same file multiple times with different CPP options.
      if oldUpdated
        then Updated newUpdated $!! newTags `Set.union` oldTags
        else Updated newUpdated $!! newTags

    fileTags =
      let tags =
            Map.fromListWith Set.union
              . map (second Set.singleton)
              . mapMaybe (ghcTagToTag dtKind dflags)
              $ getGhcTags hsModule
      in Map.map (Updated True) tags

cleanupTags :: Args -> DirtyTags -> IO Tags
cleanupTags args DirtyTags {..} = do
  newTags <- (`Map.traverseMaybeWithKey` dtTags) $ \file (Updated updated tags) -> do
    let path = T.unpack $ getTagFileName file
    -- The file might not exists even though it was updated, e.g. when .x files
    -- are preprocessed as temporary files, some tags from them might make it
    -- here.
    exists <- doesFileExist path
    if
      | exists && updated -> do
          let cleanedTags = ignoreSimilarClose . sortBy compareNAK $ Set.toList tags
          case dtKind of
            SingCTag ->
              if aExModeSearch args
                then addExCommands file cleanedTags
                else pure $ Just cleanedTags
            SingETag -> addFileOffsets file cleanedTags
      | exists && not updated -> pure . Just $ Set.toList tags
      | otherwise -> pure Nothing
  newTags `deepseq`
    pure
      Tags
        { tKind = dtKind
        , tHeaders = dtHeaders
        , tTags = newTags
        }
  where
    -- Group the same tags together so that similar ones can be eliminated.
    compareNAK t0 t1 =
      on compare tagName t0 t1
        <> on compare tagAddr t0 t1
        <> on compare tagKind t0 t1

    ignoreSimilarClose (a : b : rest)
      | tagName a == tagName b =
          if
            | a `betterThan` b -> a : ignoreSimilarClose rest
            | b `betterThan` a -> b : ignoreSimilarClose rest
            | otherwise -> a : ignoreSimilarClose (b : rest)
      | otherwise = a : ignoreSimilarClose (b : rest)
      where
        -- Prefer definitions of functions and pattern synonyms over their type
        -- signatures and data/GADT constructors over type constructors.
        x `betterThan` y =
          ( (tagKind x == TkFunction || tagKind x == TkPatternSynonym)
              && tagKind y == TkTypeSignature
          )
            || ( (tagKind x == TkDataConstructor || tagKind x == TkGADTConstructor)
                   && tagKind y == TkTypeConstructor
               )
    ignoreSimilarClose tags = tags

-- | Convert 'tagAddress' of CTags to an Ex mode search command as some editors
-- (e.g. Kate) don't support jumping to a line number and require a line match.
addExCommands :: TagFileName -> [CTag] -> IO (Maybe [CTag])
addExCommands file tags = do
  let path = T.unpack $ getTagFileName file
  tryIOError (BS.readFile path) >>= \case
    Left err -> do
      hPutStrLn stderr $ "Unexpected error: " ++ show err
      pure Nothing
    Right content -> do
      let fileLines = arrayFromList $ BS.lines content
      pure . Just $ fillExCommands fileLines tags
  where
    fillExCommands :: Array BS.ByteString -> [CTag] -> [CTag]
    fillExCommands fileLines = mapMaybe $ \tag -> case tagAddr tag of
      TagCommand {} -> Just tag
      TagLine lineNo -> do
        line <- indexArrayMaybe fileLines (lineNo - 1)
        let TagFields fields = tagFields tag
            -- Ex mode forward search command. Slashes need to be escaped.
            exCommand =
              T.concat
                ["/^", T.replace "/" "\\/" $ T.decodeUtf8Lenient line, "$/"]
        pure
          tag
            { tagAddr = TagCommand $ ExCommand exCommand
            , tagFields = TagFields $ TagField "line" (T.pack $ show lineNo) : fields
            }

-- | Add file offsets to etags from a specific file.
addFileOffsets :: TagFileName -> [ETag] -> IO (Maybe [ETag])
addFileOffsets file tags = do
  let path = T.unpack $ getTagFileName file
      addOffset !off line = (off + BS.length line + 1, (off, line))
  tryIOError (BS.readFile path) >>= \case
    Left err -> do
      hPutStrLn stderr $ "Unexpected error: " ++ show err
      pure Nothing
    Right content -> do
      let linesWithOffsets =
            arrayFromList
              . snd
              . mapAccumL addOffset 0
              . BS.lines
              $ content
      pure . Just $ fillOffsets linesWithOffsets tags
  where
    fillOffsets :: Array (Int, BS.ByteString) -> [ETag] -> [ETag]
    fillOffsets linesWithOffsets = mapMaybe $ \tag -> do
      let TagLineOff lineNo _ = tagAddr tag
      (offset, line) <- indexArrayMaybe linesWithOffsets (lineNo - 1)
      pure
        tag
          { tagAddr = TagLineOff lineNo offset
          , tagDefinition =
              -- Prevent weird characters from ending up in the TAGS file.
              TagDefinition . T.takeWhile isPrint $ T.decodeUtf8Lenient line
          }

indexArrayMaybe :: Array a -> Int -> Maybe a
indexArrayMaybe arr i
  | i >= 0 && i < sizeofArray arr = Just $ indexArray arr i
  | otherwise = Nothing

writeTags :: FilePath -> Tags -> IO ()
writeTags tagsFile Tags {..} = withFile tagsFile WriteMode $ \h ->
  BS.hPutBuilder h $ case tKind of
    SingETag -> (`Map.foldMapWithKey` tTags) $ \path ->
      ETag.formatTagsFile path . sortBy ETag.compareTags
    SingCTag -> CTag.formatTagsFile headers tTags
  where
    headers :: [Header]
    headers =
      if null tHeaders
        then defaultHeaders
        else tHeaders

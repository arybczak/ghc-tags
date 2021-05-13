module Main (main) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe (mapMaybe)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Conc (getNumCapabilities)
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Flags
import GHC.Driver.Main
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Hs
import GHC.LanguageExtensions
import GHC.Parser.Lexer
import GHC.Paths
import GHC.Platform
import GHC.Settings
import GHC.SysTools
import GHC.Types.SrcLoc
import GHC.Unit.Module.Env
import GHC.Utils.Error
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error
import qualified Control.Concurrent.Thread.Group as TG
import qualified GHC
import qualified GHC.Driver.Pipeline as DP
import qualified GHC.Parser as Parser
import qualified GHC.Utils.Outputable as Out
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import GhcTags
import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag

data Config = Config
  { configLanguage    :: Language
  , configExtensions  :: [Extension]
  , configCppIncludes :: [FilePath]
  , configExcludeDirs :: [FilePath]
  , configCppOptions  :: [String]
  }

applyConfig :: Config -> DynFlags -> DynFlags
applyConfig Config{..} = applyCppOptions . applyCppIncludes . applyExtensions . applyLanguage
  where
    applyLanguage fs = lang_set fs (Just configLanguage)

    applyExtensions fs = foldl' xopt_set fs configExtensions

    applyCppIncludes fs =
      fs { includePaths = addGlobalInclude (includePaths fs) configCppIncludes
         }

    applyCppOptions fs = foldr addOptP fs configCppOptions
      where
        addOptP opt acc =
          let ts = toolSettings acc
          in acc { toolSettings = ts { toolSettings_opt_P = opt : toolSettings_opt_P ts } }

defConfig :: Config
defConfig = Config
  { configLanguage = Haskell2010
  , configExtensions = [ BangPatterns
                       , ExplicitForAll
                       , LambdaCase
                       , MultiWayIf
                       , OverloadedLabels
                       , TypeApplications
                       ]
  , configCppIncludes = []
  , configCppOptions = [ "-DMIN_VERSION_unordered_containers(x,y,z)=1"
                       ]
  , configExcludeDirs = [ "dist"
                        , "dist-newstyle"
                        ]
  }

ghcConfig :: Config
ghcConfig = Config
  { configLanguage = Haskell2010
  , configExtensions = [ BangPatterns
                       , ExplicitForAll
                       ]
  , configCppIncludes = [ "../ghc/compiler"
                        , "../ghc/compiler/stage1/build"
                        , "../ghc/includes/dist-derivedconstants/header"
                        , "../ghc/_build/stage1/compiler/build"
                        ]
  , configCppOptions = []
  , configExcludeDirs = [ "../ghc/compiler/stage1"
                        , "../ghc/compiler/stage2"
                        ]
  }

----------------------------------------

data Updated a = Updated Bool a

data WorkerData = WorkerData
  { wdConfig :: Config
  , wdTags   :: MVar DirtyTags
  , wdTimes  :: MVar DirtyModTimes
  , wdQueue  :: TBQueue (Maybe (FilePath, UTCTime))
  }

worker :: WorkerData -> IO ()
worker WorkerData{..} = runGhc $ do
  gflags <- applyConfig wdConfig <$> getSessionDynFlags
  _ <- GHC.setSessionDynFlags gflags
  env <- getSession
  liftIO . fix $ \loop -> atomically (readTBQueue wdQueue) >>= \case
    Nothing -> pure ()
    Just (file, mtime) -> do
      --putStrLn $ "Processing " ++ file
      handle showErr $ DP.preprocess env file Nothing Nothing >>= \case
        Left errs -> report gflags errs
        Right (flags, newFile) -> do
          buffer <- hGetStringBuffer newFile
          case parseModule file flags buffer of
            PFailed pstate -> do
              let (wrns, errs) = getMessages pstate flags
              report flags wrns
              report flags errs
            POk pstate hsModule -> do
              let (wrns, errs) = getMessages pstate flags
              report flags wrns
              report flags errs
              when (isEmptyBag errs) $ do
                let path = TagFileName $ T.pack file
                modifyMVar_ wdTags $ \tags -> do
                  pure $! updateTagsWith flags hsModule tags
                modifyMVar_ wdTimes $ \times -> do
                  pure $! updateTimesWith path mtime times
      loop
  where
    showErr :: GHC.GhcException -> IO ()
    showErr = putStrLn . show

    report flags msgs =
      sequence_ [ putStrLn $ Out.showSDoc flags msg
                | msg <- pprErrMsgBagWithLoc msgs
                ]

main :: IO ()
main = do
  (conf : paths) <- getArgs
  n <- getNumCapabilities
  wd <- initWorkerData conf n

  tg <- TG.new
  mask $ \restore -> do
    replicateM_ n . TG.forkIO tg . restore $ worker wd
    -- If an exception is thrown, stop looking at files and clean up.
    handle ignoreEx . restore $ processFiles wd =<< if null paths
                                                    then listDirectory "."
                                                    else pure paths
  atomically . replicateM_ n $ writeTBQueue (wdQueue wd) Nothing
  TG.wait tg

  cleanTagMap <- withMVar (wdTags wd) cleanupTags
  writeTags tagsFile cleanTagMap
  withMVar (wdTimes wd) $ writeTimes timesFile <=< cleanupTimes cleanTagMap
  where
    tagsFile = "TAGS"
    timesFile = tagsFile <.> "mtime"

    ignoreEx :: SomeException -> IO ()
    ignoreEx _ = pure ()

    initWorkerData conf n = do
      let wdConfig = case conf of
            "ghc" -> ghcConfig
            _     -> defConfig
      wdTags  <- newMVar =<< readTags SingETag tagsFile
      wdTimes <- newMVar =<< readTimes timesFile
      wdQueue <- newTBQueueIO (fromIntegral n)
      pure WorkerData{..}

    -- Walk a list of paths recursively and process eligible source files.
    processFiles :: WorkerData -> [String] -> IO ()
    processFiles wd@WorkerData{..} = mapM_ $ \path ->
      if path `elem` configExcludeDirs wdConfig
      then pure ()
      else doesDirectoryExist path >>= \case
        True -> do
          paths <- map (path </>) <$> listDirectory path
          processFiles wd paths
        False -> when (takeExtension path `elem` haskellExtensions) $ do
          -- Source files are scanned and updated only if their mtime changed or
          -- it's not on the list.
          time <- getModificationTime path
          updateHsFile <- withMVar wdTimes $ \times -> pure $
            case TagFileName (T.pack path) `Map.lookup` times of
              Just (Updated _ oldTime) -> oldTime < time
              Nothing                  -> True
          when updateHsFile $ do
            atomically . writeTBQueue wdQueue $ Just (path, time)

    haskellExtensions = [".hs", ".hs-boot", ".lhs"]

----------------------------------------

type DirtyModTimes = Map.Map TagFileName (Updated UTCTime)
type ModTimes      = Map.Map TagFileName UTCTime

-- | Read the file with mtimes of previously processed source files.
readTimes :: FilePath -> IO DirtyModTimes
readTimes timesFile = doesFileExist timesFile >>= \case
  False -> pure Map.empty
  True  -> tryIOError (T.readFile timesFile) >>= \case
    Right content -> pure . parse Map.empty $ T.lines content
    Left err -> do
      putStrLn $ "Error while reading " ++ timesFile ++ ": " ++ show err
      pure Map.empty
  where
    parse :: DirtyModTimes -> [T.Text] -> DirtyModTimes
    parse !acc (path : mtime : rest) =
      case iso8601ParseM (T.unpack mtime) of
        Just time -> let checkedTime = Updated False time
                     in parse (Map.insert (TagFileName path) checkedTime acc) rest
        Nothing   -> parse acc rest
    parse !acc _ = acc

-- | Update an mtime of a source file with a new value.
updateTimesWith :: TagFileName -> UTCTime -> DirtyModTimes -> DirtyModTimes
updateTimesWith file time = Map.insert file (Updated True time)

-- | Check if files that were not updated exist and drop them if they don't.
cleanupTimes :: Tags -> DirtyModTimes -> IO ModTimes
cleanupTimes Tags{..} = Map.traverseMaybeWithKey $ \file -> \case
  Updated updated time
    | updated || file `Map.member` tTags -> pure $ Just time
    | otherwise -> do
        let path = T.unpack $ getTagFileName file
        doesFileExist path >>= \case
          True  -> pure $ Just time
          False -> pure Nothing

-- | Update the file with mtimes with new values.
writeTimes :: FilePath -> ModTimes -> IO ()
writeTimes timesFile times = withFile timesFile WriteMode $ \h -> do
  forM_ (Map.toList times) $ \(path, mtime) -> do
    T.hPutStrLn h $ getTagFileName path
    hPutStrLn h $ iso8601Show mtime

----------------------------------------

data DirtyTags = forall tk. DirtyTags
  { dtKind    :: SingTagKind tk
  , dtHeaders :: [CTag.Header]
  , dtTags    :: Map.Map TagFileName (Updated [Tag tk])
  }

data Tags = forall tk. Tags
  { tKind    :: SingTagKind tk
  , tHeaders :: [CTag.Header]
  , tTags    :: Map.Map TagFileName [Tag tk]
  }

readTags :: forall tk. SingTagKind tk -> FilePath -> IO DirtyTags
readTags tk tagsFile = doesFileExist tagsFile >>= \case
  False -> pure newDirtyTags
  True  -> do
    res <- tryIOError $ parseTagsFile . T.decodeUtf8 =<< BS.readFile tagsFile
    case res of
      Right (Right (headers, tags)) ->
        pure $ DirtyTags { dtKind = tk
                         , dtHeaders = headers
                         , dtTags = Map.map (Updated False) tags
                         }
      -- reading failed
      Left err -> do
        putStrLn $ "Error while reading " ++ tagsFile ++ ": " ++ show err
        pure newDirtyTags
      -- parsing failed
      Right (Left err) -> do
        putStrLn $ "Error while parsing " ++ tagsFile ++ ": " ++ show err
        pure newDirtyTags
  where
    newDirtyTags = DirtyTags { dtKind = tk
                             , dtHeaders = []
                             , dtTags = Map.empty
                             }

    parseTagsFile
      :: T.Text
      -> IO (Either String ([CTag.Header], Map.Map TagFileName [Tag tk]))
    parseTagsFile = case tk of
      SingETag -> fmap (fmap ([], )) . ETag.parseTagsFile
      SingCTag ->                      CTag.parseTagsFile

updateTagsWith :: DynFlags -> Located HsModule -> DirtyTags -> DirtyTags
updateTagsWith dflags hsModule DirtyTags{..} =
  DirtyTags { dtTags = fileTags `Map.union` dtTags
            , ..
            }
  where
    fileTags =
      let tags = Map.fromListWith (++)
               . map (second (:[]))
               . mapMaybe (ghcTagToTag dtKind dflags)
               $ getGhcTags hsModule
      in Map.map (Updated True) $!! tags

cleanupTags :: DirtyTags -> IO Tags
cleanupTags DirtyTags{..} = do
  newTags <- (`Map.traverseMaybeWithKey` dtTags) $ \file -> \case
    Updated updated tags
      | updated -> do
          let cleanedTags = ignoreSimilarClose $ sortBy compareNAK tags
          case dtKind of
            SingCTag -> pure $ Just cleanedTags
            SingETag -> addFileOffsets file cleanedTags
      | otherwise -> do
          let path = T.unpack $ getTagFileName file
          --putStrLn $ "Checking " ++ path
          doesFileExist path >>= \case
            True  -> pure $ Just tags
            False -> pure Nothing
  newTags `deepseq` pure Tags { tKind = dtKind
                              , tHeaders = dtHeaders
                              , tTags = newTags
                              }
  where
    -- Group the same tags together so that similar ones can be eliminated.
    compareNAK t0 t1 = on compare tagName t0 t1
                    <> on compare tagAddr t0 t1
                    <> on compare tagKind t0 t1

    ignoreSimilarClose (a : b : rest)
      | tagName a == tagName b =
        if | a `betterThan` b -> a : ignoreSimilarClose rest
           | b `betterThan` a -> b : ignoreSimilarClose rest
           | otherwise        -> a : ignoreSimilarClose (b : rest)
      | otherwise = a : ignoreSimilarClose (b : rest)
      where
        -- Prefer functions to type signatures and data/GADT constructors to
        -- type constructors.
        x `betterThan` y
          =  (   tagKind x == TkFunction
              && tagKind y == TkTypeSignature
             )
          || (   (tagKind x == TkDataConstructor || tagKind x == TkGADTConstructor)
              &&  tagKind y == TkTypeConstructor
             )
    ignoreSimilarClose tags = tags

-- | Add file offsets to etags from a specific file.
addFileOffsets :: TagFileName -> [ETag] -> IO (Maybe [ETag])
addFileOffsets file tags = do
  let path = T.unpack $ getTagFileName file
      addOffset !off line = (off + BS.length line + 1, (off, line))
  tryIOError (BS.readFile path) >>= \case
    Left err -> do
      putStrLn $ "Unexpected error: " ++ show err
      pure Nothing
    Right content -> do
      let linesWithOffsets = V.fromList
                           . snd
                           . mapAccumL addOffset 0
                           . BS.lines
                           $ content
      pure . Just $ fillOffsets linesWithOffsets tags
  where
    fillOffsets :: V.Vector (Int, BS.ByteString) -> [ETag] -> [ETag]
    fillOffsets linesWithOffsets = mapMaybe $ \tag -> do
      let TagLineCol lineNo _ = tagAddr tag
      (lineOffset, line) <- linesWithOffsets V.!? (lineNo - 1)
      pure tag
        { tagAddr       = TagLineCol lineNo lineOffset
        , tagDefinition =
          -- Prevent weird characters from ending up in the TAGS file.
          TagDefinition . T.takeWhile isPrint $ T.decodeUtf8 line
        }

writeTags :: FilePath -> Tags -> IO ()
writeTags tagsFile Tags{..} = withFile tagsFile WriteMode $ \h ->
  BS.hPutBuilder h $ case tKind of
    SingETag -> (`Map.foldMapWithKey` tTags) $ \path ->
      ETag.formatTagsFile path . sortBy ETag.compareTags
    SingCTag -> CTag.formatTagsFile tHeaders $
      Map.map (sortBy CTag.compareTags) tTags

----------------------------------------

runGhc :: Ghc a -> IO a
runGhc m = do
  env <- liftIO $ do
    mySettings <- initSysTools libdir
    myLlvmConfig <- lazyInitLlvmConfig libdir
    dflags <- threadSafeInitDynFlags (defaultDynFlags mySettings myLlvmConfig)
    newHscEnv dflags
  ref <- newIORef env
  unGhc (GHC.withCleanupSession m) (Session ref)
  where
    threadSafeInitDynFlags dflags = do
      let -- We can't build with dynamic-too on Windows, as labels before the
          -- fork point are different depending on whether we are building
          -- dynamically or not.
          platformCanGenerateDynamicToo
              = platformOS (targetPlatform dflags) /= OSMinGW32
      refCanGenerateDynamicToo <- newIORef platformCanGenerateDynamicToo
      refNextTempSuffix <- newIORef 0
      refFilesToClean <- newIORef emptyFilesToClean
      refDirsToClean <- newIORef Map.empty
      refGeneratedDumps <- newIORef Set.empty
      refRtldInfo <- newIORef Nothing
      refRtccInfo <- newIORef Nothing
      wrapperNum <- newIORef emptyModuleEnv
      pure dflags
        { canGenerateDynamicToo = refCanGenerateDynamicToo
        , nextTempSuffix = refNextTempSuffix
        , filesToClean   = refFilesToClean
        , dirsToClean    = refDirsToClean
        , generatedDumps = refGeneratedDumps
        , nextWrapperNum = wrapperNum
        , rtldInfo       = refRtldInfo
        , rtccInfo       = refRtccInfo
        }

parseModule
  :: FilePath
  -> DynFlags
  -> StringBuffer
  -> ParseResult (Located HsModule)
parseModule filename flags buffer =
  unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    parseState = mkPState flags buffer location

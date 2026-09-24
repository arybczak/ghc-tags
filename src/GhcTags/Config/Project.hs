{-# LANGUAGE CPP #-}
module GhcTags.Config.Project where

import Control.Monad
import Data.List
import Data.YAML
import Data.YAML.Event hiding (Scalar)
import Data.YAML.Schema
import Data.YAML.Token
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.LanguageExtensions
import GHC.Settings
import System.Directory
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | A language extension to either enable or disable.
data ExtensionFlag
  = EnableExtension Extension
  | DisableExtension Extension
  deriving (Eq, Show)

data ProjectConfig = ProjectConfig
  { pcSourcePaths  :: [FilePath]
  , pcExcludePaths :: [FilePath]
  , pcLanguage     :: Language
  , pcExtensions   :: [ExtensionFlag]
  , pcCppIncludes  :: [FilePath]
  , pcCppOptions   :: [String]
  }

defaultProjectConfig :: ProjectConfig
defaultProjectConfig = ProjectConfig
  { pcSourcePaths  = [ "."
                     ]
  , pcExcludePaths = [ ".stack-work"
                     , "dist"
                     , "dist-newstyle"
                     ]
  , pcLanguage     = Haskell2010
  , pcExtensions   = map EnableExtension
                     [ BangPatterns
                     , BinaryLiterals
                     , BlockArguments
                     , CApiFFI
                     , ExplicitForAll
                     , ExplicitNamespaces
                     , GADTSyntax
                     , ImportQualifiedPost
                     , LambdaCase
                     , LinearTypes
                     , MagicHash
#if __GLASGOW_HASKELL__ >= 912
                     , MultilineStrings
#endif
                     , MultiWayIf
                     , NumericUnderscores
                     , OverloadedLabels
                     , PatternSynonyms
                     , QualifiedDo
                     , QuasiQuotes
                     , TemplateHaskellQuotes
                     , TypeApplications
                     , UnicodeSyntax
                     ]
  , pcCppIncludes = []
  , pcCppOptions  = []
  }

-- | Configuration files probed when '--config' is not given, in the order of
-- precedence.
defaultConfigFiles :: [FilePath]
defaultConfigFiles = ["ghc-tags.yaml", ".ghc-tags.yaml"]

-- | Read the project configurations from the given file or, when there is
-- none, from the first of 'defaultConfigFiles' that exists. Return 'Nothing'
-- when the file exists and cannot be parsed.
getProjectConfigs :: Maybe FilePath -> IO (Maybe [ProjectConfig])
getProjectConfigs mfile = resolve >>= \case
  Nothing   -> pure $ Just [defaultProjectConfig]
  Just file -> do
    content <- BL.fromStrict <$> BS.readFile file
    case decode content of
      Left (pos, e) -> do
        hPutStr stderr $ prettyError file content pos e
        pure Nothing
      Right pcs -> pure $ Just pcs
  where
    -- HsYAML counts columns from 0, but editors count them from 1.
    prettyError :: FilePath -> BL.ByteString -> Pos -> String -> String
    prettyError file content pos e
      | posCharOffset pos < 0 = file ++ ": " ++ e ++ "\n"
      | otherwise             = file ++ ":" ++ show (posLine pos)
                             ++ ":" ++ show (posColumn pos + 1)
                             ++ ": " ++ e ++ "\n" ++ excerpt
      where
        excerpt :: String
        excerpt = unlines . drop 1 . lines $ prettyPosWithSource pos content ""

    resolve :: IO (Maybe FilePath)
    resolve = case mfile of
      Just file -> doesFileExist file >>= \case
        True  -> pure $ Just file
        False -> pure Nothing
      Nothing -> filterM doesFileExist defaultConfigFiles >>= \case
        []            -> pure Nothing
        file : others -> do
          forM_ others $ \other -> hPutStrLn stderr $
            "Warning: both " ++ file ++ " and " ++ other
            ++ " exist, reading " ++ file
          pure $ Just file

-- | Render the configuration by hand, because the YAML encoder sorts the keys
-- of a mapping.
ppProjectConfig :: ProjectConfig -> String
ppProjectConfig ProjectConfig{..} = concat
  [ field "source_paths"  $ map T.pack pcSourcePaths
  , field "exclude_paths" $ map T.pack pcExcludePaths
  , field "language"      . T.pack $ show pcLanguage
  , field "extensions"    $ map showExtensionFlag pcExtensions
  , field "cpp_includes"  $ map T.pack pcCppIncludes
  , field "cpp_options"   $ map T.pack pcCppOptions
  ]
  where
    field :: ToYAML a => T.Text -> a -> String
    field key value = T.unpack key ++ ":" ++ separator ++ rendered
      where
        node :: Node ()
        node = toYAML value

        separator :: String
        separator = case node of
          Sequence _ _ (_ : _) -> "\n"
          _                    -> " "

        rendered :: String
        rendered = T.unpack . T.decodeUtf8 . BL.toStrict
                 $ encodeNode' encoder UTF8 [Doc node]

    -- The core encoder quotes every string with a dash, e.g. dist-newstyle.
    encoder :: SchemaEncoder
    encoder = setScalarStyle scalar coreSchemaEncoder
      where
        scalar :: Scalar -> Either String (Tag, ScalarStyle, T.Text)
        scalar = \case
          SStr t | isPlain t -> Right (untagged, Plain, t)
          s                  -> schemaEncoderScalar coreSchemaEncoder s

        isPlain :: T.Text -> Bool
        isPlain t = case T.uncons t of
          Just (c, _) -> c `notElem` ['-', ' ']
                      && T.last t /= ' '
                      && T.all (\x -> x == '-' || isPlainChar x) t
                      && not (isAmbiguous coreSchemaResolver t)
          Nothing     -> False

adjustDynFlags :: ProjectConfig -> DynFlags -> DynFlags
adjustDynFlags ProjectConfig{..} = applyCppOptions
                                 . applyCppIncludes
                                 . applyExtensions
                                 . applyLanguage
  where
    applyLanguage fs = lang_set fs (Just pcLanguage)

    applyExtensions fs = foldl' setExtension fs pcExtensions
      where
        setExtension :: DynFlags -> ExtensionFlag -> DynFlags
        setExtension acc = \case
          EnableExtension  ext -> xopt_set   acc ext
          DisableExtension ext -> xopt_unset acc ext

    applyCppIncludes fs =
      fs { includePaths = addGlobalInclude (includePaths fs) pcCppIncludes
         }

    applyCppOptions fs = foldr addOptP fs pcCppOptions
      where
        addOptP opt acc =
          let ts = toolSettings acc
          in acc { toolSettings = ts
                   { toolSettings_opt_P = opt : toolSettings_opt_P ts
                   }
                 }

----------------------------------------
-- YAML instances

instance FromYAML ProjectConfig where
  parseYAML = withMapping $ \m -> do
    fields <- fmap Map.fromList . forM (Map.toList m) $ \case
      (Scalar _ (SStr key), value) -> pure (key, value)
      (key, _)                     -> mismatch "a string" key
    checkUnknownKeys $ Map.keys fields
    let field :: (Node Pos -> Parser a) -> T.Text -> (ProjectConfig -> a) -> Parser a
        field parse key def = maybe (pure $ def defaultProjectConfig) parse
                                    (key `Map.lookup` fields)
    pcSourcePaths  <- field (listOf string)             "source_paths"  pcSourcePaths
    pcExcludePaths <- field (listOf string)             "exclude_paths" pcExcludePaths
    pcLanguage     <- field parseLanguage               "language"      pcLanguage
    pcExtensions   <- field (listOf parseExtensionFlag) "extensions"    pcExtensions
    pcCppIncludes  <- field (listOf string)             "cpp_includes"  pcCppIncludes
    pcCppOptions   <- field (listOf string)             "cpp_options"   pcCppOptions
    pure ProjectConfig{..}
    where
      checkUnknownKeys :: [T.Text] -> Parser ()
      checkUnknownKeys keys = case keys \\ projectConfigKeys of
        []  -> pure ()
        [k] -> fail $ "unknown key: "  ++ T.unpack k
        ks  -> fail $ "unknown keys: " ++ intercalate ", " (map T.unpack ks)

      string :: Node Pos -> Parser String
      string = text $ pure . T.unpack

      parseLanguage :: Node Pos -> Parser Language
      parseLanguage = text $ \t -> case readLanguage t of
        Just lang -> pure lang
        Nothing   -> fail $ "unknown language: " ++ T.unpack t

      parseExtensionFlag :: Node Pos -> Parser ExtensionFlag
      parseExtensionFlag = text $ \t -> case readExtensionFlag t of
        Just ext -> pure ext
        Nothing  -> fail $ "unknown extension: " ++ T.unpack t

      -- The with* functions of HsYAML name the kinds of nodes with YAML tags,
      -- e.g. "expected !!seq instead of !!int", so a mismatch is reported here.

      withMapping :: (Mapping Pos -> Parser a) -> Node Pos -> Parser a
      withMapping parse node = case node of
        Mapping{} -> withMap "a mapping" parse node
        _         -> mismatch "a mapping" node

      listOf :: (Node Pos -> Parser a) -> Node Pos -> Parser [a]
      listOf parse node = case node of
        Sequence{} -> withSeq "a list" (mapM parse) node
        _          -> mismatch "a list" node

      text :: (T.Text -> Parser a) -> Node Pos -> Parser a
      text parse node = case node of
        Scalar _ (SStr _) -> withStr "a string" parse node
        _                 -> mismatch "a string" node

      mismatch :: String -> Node Pos -> Parser a
      mismatch expected node = failAtNode node $
        "expected " ++ expected ++ ", but got " ++ actual
        where
          actual :: String
          actual = case node of
            Scalar _ SNull         -> "an empty value"
            Scalar _ (SBool _)     -> "a boolean"
            Scalar _ (SInt _)      -> "a number"
            Scalar _ (SFloat _)    -> "a number"
            Scalar _ (SStr _)      -> "a string"
            Scalar _ SUnknown{}    -> "a value with an unknown tag"
            Mapping{}              -> "a mapping"
            Sequence{}             -> "a list"
            Anchor{}               -> "an anchor"

projectConfigKeys :: [T.Text]
projectConfigKeys = [ "source_paths"
                    , "exclude_paths"
                    , "language"
                    , "extensions"
                    , "cpp_includes"
                    , "cpp_options"
                    ]

----------------------------------------
-- Utils

readLanguage :: T.Text -> Maybe Language
readLanguage = \case
  "Haskell98" -> Just Haskell98
  "Haskell2010" -> Just Haskell2010
  "GHC2021" -> Just GHC2021
  "GHC2024" -> Just GHC2024
  _ -> Nothing

showExtensionFlag :: ExtensionFlag -> T.Text
showExtensionFlag = \case
  EnableExtension  ext -> showExtension ext
  DisableExtension ext -> "No" <> showExtension ext
  where
    showExtension :: Extension -> T.Text
    showExtension Cpp = "CPP"
    showExtension ext = T.pack $ show ext

-- | Parse an extension name. A @No@ prefix disables the extension instead of
-- enabling it. The prefix is only stripped if the full name is not an extension
-- itself, so @NondecreasingIndentation@ keeps its meaning.
readExtensionFlag :: T.Text -> Maybe ExtensionFlag
readExtensionFlag name = case readExtension name of
  Just ext -> Just $ EnableExtension ext
  Nothing  -> DisableExtension <$> (readExtension =<< T.stripPrefix "No" name)
  where
    readExtension :: T.Text -> Maybe Extension
    readExtension ext = ext `Map.lookup` exts
      where
        exts :: Map.Map T.Text Extension
        exts = Map.fromList . (("CPP", Cpp) :)
                            . map (\e -> (T.pack $ show e, e))
                            $ filter (/= Cpp) [minBound..maxBound]

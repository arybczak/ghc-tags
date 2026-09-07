{-# LANGUAGE CPP #-}
module GhcTags.Config.Project where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.List
import Data.Ord
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.LanguageExtensions
import GHC.Settings
import System.Directory
import System.IO
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y

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

-- | Read the project configurations from a file. Return 'Nothing' when the file
-- exists and cannot be parsed.
getProjectConfigs :: FilePath -> IO (Maybe [ProjectConfig])
getProjectConfigs file = doesFileExist file >>= \case
  True  -> Y.decodeAllFileEither file >>= \case
    Left e  -> do
      hPutStrLn stderr $ file ++ ": " ++ Y.prettyPrintParseException e
      pure Nothing
    Right pcs -> pure $ Just pcs
  False -> pure $ Just [defaultProjectConfig]

ppProjectConfig :: ProjectConfig -> String
ppProjectConfig = BS.unpack . Y.encodePretty conf
  where
    conf = Y.setConfCompare (keyOrder projectConfigKeys) Y.defConfig

    keyOrder :: [T.Text] -> T.Text -> T.Text -> Ordering
    keyOrder ks = comparing $ \k -> fromMaybe maxBound (elemIndex k ks)

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
-- JSON instances

instance ToJSON ProjectConfig where
  toJSON ProjectConfig{..} = object
    [ "source_paths"  .= pcSourcePaths
    , "exclude_paths" .= pcExcludePaths
    , "language"      .= show pcLanguage
    , "extensions"    .= map showExtensionFlag pcExtensions
    , "cpp_includes"  .= pcCppIncludes
    , "cpp_options"   .= pcCppOptions
    ]

instance FromJSON ProjectConfig where
  parseJSON (Object v) = do
    checkUnknownKeys . map K.toText $ K.keys v
    pcSourcePaths  <- def pcSourcePaths  <$> v .:! "source_paths"
    pcExcludePaths <- def pcExcludePaths <$> v .:! "exclude_paths"
    pcLanguage     <- def pcLanguage     <$> explicitParseFieldMaybe'
                                               parseLanguage v
                                               "language"
    pcExtensions   <- def pcExtensions   <$> explicitParseFieldMaybe'
                                               (listParser parseExtensionFlag) v
                                               "extensions"
    pcCppIncludes  <- def pcCppIncludes  <$> v .:! "cpp_includes"
    pcCppOptions   <- def pcCppOptions   <$> v .:! "cpp_options"
    pure ProjectConfig{..}
    where
      def f = fromMaybe (f defaultProjectConfig)

      checkUnknownKeys :: [T.Text] -> Parser ()
      checkUnknownKeys keys = case keys \\ projectConfigKeys of
        []  -> pure ()
        [k] -> fail $ "unknown key: "  ++ T.unpack k
        ks  -> fail $ "unknown keys: " ++ intercalate ", " (map T.unpack ks)

      parseLanguage :: Value -> Parser Language
      parseLanguage (String t) = case readLanguage t of
        Just lang -> pure lang
        Nothing   -> fail $ "unknown language: " ++ T.unpack t
      parseLanguage inv = typeMismatch "String" inv

      parseExtensionFlag :: Value -> Parser ExtensionFlag
      parseExtensionFlag (String t) = case readExtensionFlag t of
        Just ext -> pure ext
        Nothing  -> fail $ "unknown extension: " ++ T.unpack t
      parseExtensionFlag inv = typeMismatch "String" inv

  parseJSON v = prependFailure "parsing project configuration failed: " $
    typeMismatch "Object" v

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

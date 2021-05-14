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
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y

data ProjectConfig = ProjectConfig
  { pcSourcePaths  :: [FilePath]
  , pcExcludePaths :: [FilePath]
  , pcLanguage     :: Language
  , pcExtensions   :: [Extension]
  , pcCppIncludes  :: [FilePath]
  , pcCppOptions   :: [String]
  }

defaultProjectConfig :: ProjectConfig
defaultProjectConfig = ProjectConfig
  { pcSourcePaths  = [ "."
                     ]
  , pcExcludePaths = [ "dist"
                     , "dist-newstyle"
                     ]
  , pcLanguage     = Haskell2010
  , pcExtensions   = [ BangPatterns
                     , ExplicitForAll
                     , LambdaCase
                     , MultiWayIf
                     , OverloadedLabels
                     , TypeApplications
                     ]
  , pcCppIncludes = []
  , pcCppOptions  = []
  }

getProjectConfigs :: FilePath -> IO [ProjectConfig]
getProjectConfigs file = doesFileExist file >>= \case
  True  -> Y.decodeAllFileEither file >>= \case
    Left e  -> do
      putStrLn $ file ++ ": " ++ Y.prettyPrintParseException e
      pure []
    Right pcs -> pure pcs
  False -> pure [defaultProjectConfig]

ppProjectConfig :: ProjectConfig -> BS.ByteString
ppProjectConfig = Y.encodePretty conf
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

    applyExtensions fs = foldl' xopt_set fs pcExtensions

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
    , "extensions"    .= map showExtension pcExtensions
    , "cpp_includes"  .= pcCppIncludes
    , "cpp_options"   .= pcCppOptions
    ]

instance FromJSON ProjectConfig where
  parseJSON (Object v) = do
    checkUnknownKeys $ HM.keys v
    pcSourcePaths  <- def pcSourcePaths  <$> v .:! "source_paths"
    pcExcludePaths <- def pcExcludePaths <$> v .:! "exclude_paths"
    pcLanguage     <- def pcLanguage     <$> explicitParseFieldMaybe'
                                               parseLanguage v
                                               "language"
    pcExtensions   <- def pcExtensions   <$> explicitParseFieldMaybe'
                                               (listParser parseExtension) v
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

      parseExtension :: Value -> Parser Extension
      parseExtension (String t) = case readExtension t of
        Just ext -> pure ext
        Nothing  -> fail $ "unknown extension: " ++ T.unpack t
      parseExtension inv = typeMismatch "String" inv

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
readLanguage "Haskell98"   = Just Haskell98
readLanguage "Haskell2010" = Just Haskell2010
readLanguage _             = Nothing

showExtension :: Extension -> T.Text
showExtension Cpp = "CPP"
showExtension ext = T.pack $ show ext

readExtension :: T.Text -> Maybe Extension
readExtension ext = ext `Map.lookup` exts
  where
    exts :: Map.Map T.Text Extension
    exts = Map.fromList . (("CPP", Cpp) :)
                        . map (\e -> (T.pack $ show e, e))
                        $ filter (/= Cpp) [minBound..maxBound]

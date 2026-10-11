{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GhcTags.Config.Project where

import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.Generics (Generic)
import GHC.LanguageExtensions
import GHC.Settings
import System.Directory
import System.IO
import Yamlet qualified as Y

-- | A language extension to either enable or disable.
data ExtensionFlag
  = EnableExtension Extension
  | DisableExtension Extension
  deriving stock (Eq, Show)

data ProjectConfig = ProjectConfig
  { pcSourcePaths :: [FilePath]
  , pcExcludePaths :: [FilePath]
  , pcLanguage :: Language
  , pcExtensions :: [ExtensionFlag]
  , pcCppIncludes :: [FilePath]
  , pcCppOptions :: [String]
  }
  deriving stock (Generic)
  deriving (Y.FromYaml, Y.ToYaml) via Y.GenericYaml ProjectConfig

-- | The keys are the field names without the prefix in snake case, e.g.
-- @source_paths@. A missing key takes its value from 'defaultProjectConfig'.
instance Y.GenericYamlOptions ProjectConfig where
  yamlOptions =
    Y.defaultYamlOptions
      { Y.fieldLabelModifier = Y.snakeCase . drop (length prefix)
      }
    where
      prefix :: String
      prefix = "pc"

  yamlDefault = Just defaultProjectConfig

defaultProjectConfig :: ProjectConfig
defaultProjectConfig =
  ProjectConfig
    { pcSourcePaths =
        [ "."
        ]
    , pcExcludePaths =
        [ ".stack-work"
        , "dist"
        , "dist-newstyle"
        ]
    , pcLanguage = Haskell2010
    , pcExtensions =
        map EnableExtension $
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
          ]
            ++ multilineStrings
            ++ [ MultiWayIf
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
    , pcCppOptions = []
    }

-- At the top level, because fourmolu cannot format CPP inside a where clause.
multilineStrings :: [Extension]
#if __GLASGOW_HASKELL__ >= 912
multilineStrings = [MultilineStrings]
#else
multilineStrings = []
#endif

-- | Configuration files probed when '--config' is not given, in the order of
-- precedence.
defaultConfigFiles :: [FilePath]
defaultConfigFiles = ["ghc-tags.yaml", ".ghc-tags.yaml"]

-- | Read the project configurations from the given file or, when there is
-- none, from the first of 'defaultConfigFiles' that exists. Return 'Nothing'
-- when the file exists and cannot be parsed.
getProjectConfigs :: Maybe FilePath -> IO (Maybe [ProjectConfig])
getProjectConfigs mfile =
  resolve >>= \case
    Nothing -> pure $ Just [defaultProjectConfig]
    Just file ->
      Y.decodeAllFile @ProjectConfig file >>= \case
        Left errs -> do
          mapM_ (hPutStrLn stderr . Y.prettyError file) errs
          pure Nothing
        Right pcs -> pure $ Just pcs
  where
    resolve :: IO (Maybe FilePath)
    resolve = case mfile of
      Just file ->
        doesFileExist file >>= \case
          True -> pure $ Just file
          False -> pure Nothing
      Nothing ->
        filterM doesFileExist defaultConfigFiles >>= \case
          [] -> pure Nothing
          file : others -> do
            forM_ others $ \other ->
              hPutStrLn stderr $
                "Warning: both "
                  ++ file
                  ++ " and "
                  ++ other
                  ++ " exist, reading "
                  ++ file
            pure $ Just file

ppProjectConfig :: ProjectConfig -> String
ppProjectConfig = T.unpack . Y.encodeText

adjustDynFlags :: ProjectConfig -> DynFlags -> DynFlags
adjustDynFlags ProjectConfig {..} =
  applyCppOptions
    . applyCppIncludes
    . applyExtensions
    . applyLanguage
  where
    applyLanguage fs = lang_set fs (Just pcLanguage)

    applyExtensions fs = foldl' setExtension fs pcExtensions
      where
        setExtension :: DynFlags -> ExtensionFlag -> DynFlags
        setExtension acc = \case
          EnableExtension ext -> xopt_set acc ext
          DisableExtension ext -> xopt_unset acc ext

    applyCppIncludes fs =
      fs
        { includePaths = addGlobalInclude (includePaths fs) pcCppIncludes
        }

    applyCppOptions fs = foldr addOptP fs pcCppOptions
      where
        addOptP opt acc =
          let ts = toolSettings acc
          in acc
               { toolSettings =
                   ts
                     { toolSettings_opt_P = opt : toolSettings_opt_P ts
                     }
               }

----------------------------------------
-- YAML instances

instance Y.FromYaml Language where
  parseYaml = Y.withText $ \t -> case readLanguage t of
    Just lang -> pure lang
    Nothing -> fail $ "unknown language: " ++ T.unpack t

instance Y.ToYaml Language where
  toYaml = Y.toYaml . show

instance Y.FromYaml ExtensionFlag where
  parseYaml = Y.withText $ \t -> case readExtensionFlag t of
    Just ext -> pure ext
    Nothing -> fail $ "unknown extension: " ++ T.unpack t

instance Y.ToYaml ExtensionFlag where
  toYaml = Y.toYaml . showExtensionFlag

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
  EnableExtension ext -> showExtension ext
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
  Nothing -> DisableExtension <$> (readExtension =<< T.stripPrefix "No" name)
  where
    readExtension :: T.Text -> Maybe Extension
    readExtension ext = ext `Map.lookup` exts
      where
        exts :: Map.Map T.Text Extension
        exts =
          Map.fromList
            . (("CPP", Cpp) :)
            . map (\e -> (T.pack $ show e, e))
            $ filter (/= Cpp) [minBound .. maxBound]

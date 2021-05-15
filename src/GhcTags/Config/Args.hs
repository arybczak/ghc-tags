{-# LANGUAGE ApplicativeDo #-}
module GhcTags.Config.Args where

import Data.Version
import Options.Applicative
import Options.Applicative.Help.Pretty

import GhcTags.Config.Project
import Paths_ghc_tags (version)

-- | Generate either ctags or etags.
data TagType = CTags | ETags
  deriving Show

defaultOutputFile :: TagType -> FilePath
defaultOutputFile CTags = "tags"
defaultOutputFile ETags = "TAGS"

-- | Get source paths from the configuration file or command line arguments.
data SourcePaths
  = SourceArgs [FilePath]
  | ConfigFile FilePath
  deriving Show

data Args = Args
  { aTagType      :: TagType
  , aTagFile      :: FilePath
  , aThreads      :: Int
  , aSourcePaths  :: SourcePaths
  } deriving Show

argsParser :: Int -> Parser Args
argsParser defaultThreads = do
  aTagType     <- ctags <|> etags
  aTagFile0    <- tagFile
  aThreads     <- threads
  aSourcePaths <- (SourceArgs <$> sourcePaths) <|> (ConfigFile <$> configFile)
  pure $ Args { aTagFile = if null aTagFile0
                           then defaultOutputFile aTagType
                           else aTagFile0
              , ..
              }
  where
    ctags :: Parser TagType
    ctags = flag' CTags $ long "ctags"
                       <> short 'c'
                       <> help "Generate CTAGS file"

    etags :: Parser TagType
    etags = flag' ETags $ long "etags"
                       <> short 'e'
                       <> help "Generate ETAGS file"

    tagFile :: Parser FilePath
    tagFile = strOption $ long "output"
                       <> short 'o'
                       <> metavar "FILE"
                       <> help "Output file"
                       <> value ""
                       <> showDefaultWith (const "TAGS (etags) or tags (ctags)")

    configFile :: Parser FilePath
    configFile = strOption $ long "config"
                          <> metavar "FILE"
                          <> value "ghc-tags.yaml"
                          <> showDefaultWith id
                          <> help "Configuration file"

    sourcePaths :: Parser [FilePath]
    sourcePaths = some . argument str $ metavar "<source paths...>"

    threads :: Parser Int
    threads = option auto $ long "threads"
                         <> short 'j'
                         <> metavar "NUMBER"
                         <> value defaultThreads
                         <> showDefault
                         <> help "Number of threads to use"

parseArgs :: Int -> [String] -> IO Args
parseArgs defaultThreads = handleParseResult . execParserPure defaultPrefs opts
  where
    opts = info
      (argsParser defaultThreads <**> defaultConfigFlag <**> versionFlag <**> helper)
      (fullDesc <> progDescDoc (Just $ line <> configHint))

    configHint =
      text "If the given configuration file doesn't exist, a default one is used."

    defaultConfigFlag = infoOption (ppProjectConfig defaultProjectConfig)
       $ long "default-config"
      <> help "Show a default configuration file"

    versionFlag = infoOption (showVersion version)
       $ long "version"
      <> help "Show version"

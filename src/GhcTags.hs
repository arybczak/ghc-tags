{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Bag
import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.List
import ErrUtils
import GHC
import GHC.Paths
import GhcPlugins
import Lexer
import StringBuffer
import System.Directory
import System.Environment
import System.FilePath
import qualified Control.Concurrent.Thread.Group as TG
import qualified DriverPipeline as DP
import qualified GHC.LanguageExtensions as LE
import qualified Parser

parse
  :: FilePath
  -> DynFlags
  -> StringBuffer
  -> ParseResult (Located (HsModule GhcPs))
parse filename flags sb =
  unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    parseState = mkPState flags sb location

preprocess
  :: HscEnv
  -> FilePath
  -> Maybe Phase
  -> IO (Either ErrorMessages (DynFlags, FilePath))
preprocess env file mphase =
#if __GLASGOW_HASKELL__ >= 808
      DP.preprocess env file Nothing mphase
#else
      Right <$> DP.preprocess env (file, mphase)
#endif

worker :: TQueue (Maybe FilePath) -> IO ()
worker queue = runGhc (Just libdir) $ do
  xflags <- (`xopt_set` LE.OverloadedLabels)
          . (`xopt_set` LE.ExplicitForAll)
          . (`xopt_set` LE.LambdaCase)
          . (`xopt_set` LE.TypeApplications)
          . (`xopt_set` LE.MultiWayIf)
          . (`xopt_set` LE.BangPatterns)
        <$> getSessionDynFlags
  _ <- setSessionDynFlags xflags
  env <- getSession
  liftIO . fix $ \loop -> atomically (readTQueue queue) >>= \case
    Nothing -> return ()
    Just file -> do
      --putStrLn $ "Processing " ++ file
      preprocess env file Nothing >>= \case
        Left errs -> report xflags errs
        Right (flags, newFile) -> do
          s <- hGetStringBuffer newFile
          case parse file flags s of
            PFailed _ lc err -> do
              report flags $ unitBag $ mkPlainErrMsg flags lc err
            POk pstate _hsModule -> do
              let (wrns, errs) = getMessages pstate flags
              report flags wrns
              report flags errs
      loop
  where
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <- pprErrMsgBagWithLoc msgs
        ]

main :: IO ()
main = do
  let n = 4
  dirs <- getArgs

  -- Start 4 workers that will parse files in parallel
  tg <- TG.new
  queue <- newTQueueIO
  replicateM_ n . TG.forkIO tg $ worker queue

  forM_ dirs $ \baseDir -> (`fix` baseDir) $ \loop dir -> do
    files <- map (dir </>) <$> listDirectory dir
    forM_ files $ \file -> doesDirectoryExist file >>= \case
      True  -> loop file
      False -> when (".hs" `isSuffixOf` file) $ do
        atomically . writeTQueue queue $ Just file

  -- Send Nothing to all workers so they finish and wait
  replicateM_ n . atomically $ writeTQueue queue Nothing
  TG.wait tg

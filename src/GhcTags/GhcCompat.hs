{-# LANGUAGE CPP #-}

module GhcTags.GhcCompat
  ( runGhc
  , parseModule
  , recFieldNames
  , pattern SpecSigE
  ) where

import Data.IORef
import GHC qualified
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser
import GHC.Driver.Main
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Hs
import GHC.Parser qualified as Parser
import GHC.Parser.Lexer
import GHC.Paths
import GHC.SysTools
import GHC.Types.SrcLoc

parseModule
  :: FilePath
  -> DynFlags
  -> StringBuffer
  -> ParseResult (Located (HsModule GhcPs))
parseModule filename flags buffer = unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    parseState = initParserState (initParserOpts flags) buffer location

runGhc :: Ghc a -> IO a
runGhc m = do
  dflags <- initDynFlags . defaultDynFlags =<< initSysTools libdir
  env <- newHscEnv libdir dflags
  ref <- newIORef env
  unGhc (GHC.withCleanupSession m) (Session ref)

#if __GLASGOW_HASKELL__ >= 914
recFieldNames :: LHsConDeclRecField GhcPs -> [LFieldOcc GhcPs]
recFieldNames (L _ HsConDeclRecField { cdrf_names }) = cdrf_names
#else
recFieldNames :: LConDeclField GhcPs -> [LFieldOcc GhcPs]
recFieldNames (L _ ConDeclField { cd_fld_names }) = cd_fld_names
#endif

#if __GLASGOW_HASKELL__ < 914
-- | A stand-in for the constructor that GHC 9.14 added. It never matches.
pattern SpecSigE :: () -> Sig GhcPs
pattern SpecSigE x <- (const Nothing -> Just x)
#endif

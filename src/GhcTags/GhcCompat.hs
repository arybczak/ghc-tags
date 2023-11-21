{-# OPTIONS_GHC -Wno-missing-fields #-}
module GhcTags.GhcCompat
  ( runGhc
  , parseModule
  ) where

import Data.IORef
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser
import GHC.Driver.Main
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Hs
import GHC.Parser.Lexer
import GHC.Paths
import GHC.Platform
import GHC.Settings
import GHC.Settings.Config
import GHC.Settings.Utils
import GHC.SysTools.BaseDir
import GHC.Types.SrcLoc
import GHC.Utils.Fingerprint
import GHC.Utils.TmpFs
import System.Directory
import System.FilePath
import qualified Data.Map.Strict as Map
import qualified GHC
import qualified GHC.Parser as Parser

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
  env <- liftIO $ do
    mySettings <- compatInitSettings libdir
    dflags <- threadSafeInitDynFlags (defaultDynFlags mySettings)
    newHscEnv libdir dflags
  ref <- newIORef env
  unGhc (GHC.withCleanupSession m) (Session ref)

----------------------------------------
-- Internal

-- | Adjusted version of 'GHC.Driver.Session.initDynFlags' that doesn't check
-- for colors as it's not thread safe.
threadSafeInitDynFlags :: DynFlags -> IO DynFlags
threadSafeInitDynFlags dflags = do
  refRtldInfo <- newIORef Nothing
  refRtccInfo <- newIORef Nothing
  tmpdir      <- liftIO getTemporaryDirectory
  pure dflags
    { rtldInfo = refRtldInfo
    , rtccInfo = refRtccInfo
    , tmpDir   = TempDir tmpdir
    }

-- | Stripped version of 'GHC.Settings.IO.initSettings' that ignores the
-- @platformConstants@ file as it's irrelevant for parsing.
compatInitSettings :: FilePath -> IO Settings
compatInitSettings top_dir = do
  let installed :: FilePath -> FilePath
      installed file = top_dir </> file
      libexec :: FilePath -> FilePath
      libexec file = top_dir </> "bin" </> file
      settingsFile = installed "settings"

      readFileSafe :: FilePath -> IO String
      readFileSafe path = doesFileExist path >>= \case
        True -> readFile path
        False -> error $ "Missing file: " ++ path

  settingsStr <- readFileSafe settingsFile
  settingsList <- case maybeReadFuzzy settingsStr of
    Just s -> pure s
    Nothing -> error $ "Can't parse " ++ show settingsFile
  let mySettings = Map.fromList settingsList
  -- See Note [Settings file] for a little more about this file. We're
  -- just partially applying those functions and throwing 'Left's; they're
  -- written in a very portable style to keep ghc-boot light.
  let getBooleanSetting :: String -> IO Bool
      getBooleanSetting key = either error pure $
        getRawBooleanSetting settingsFile mySettings key

  -- On Windows, by mingw is often distributed with GHC,
  -- so we look in TopDir/../mingw/bin,
  -- as well as TopDir/../../mingw/bin for hadrian.
  -- But we might be disabled, in which we we don't do that.
  -- useInplaceMinGW <- getBooleanSetting "Use inplace MinGW toolchain"
  useInplaceMinGW <- pure True -- compatibility with GHC < 9.4
  -- see Note [topdir: How GHC finds its files]
  -- NB: top_dir is assumed to be in standard Unix
  -- format, '/' separated
  mtool_dir <- findToolDir useInplaceMinGW top_dir
        -- see Note [tooldir: How GHC finds mingw on Windows]

  let getSetting key = either error pure $
        getRawFilePathSetting top_dir settingsFile mySettings key
      getToolSetting :: String -> IO String
      getToolSetting key = expandToolDir useInplaceMinGW mtool_dir <$> getSetting key

  -- On Windows, mingw is distributed with GHC,
  -- so we look in TopDir/../mingw/bin,
  -- as well as TopDir/../../mingw/bin for hadrian.
  -- It would perhaps be nice to be able to override this
  -- with the settings file, but it would be a little fiddly
  -- to make that possible, so for now you can't.
  cc_prog <- getToolSetting "C compiler command"
  cc_args_str <- getSetting "C compiler flags"
  cxx_args_str <- getSetting "C++ compiler flags"
  gccSupportsNoPie <- getBooleanSetting "C compiler supports -no-pie"
  cpp_prog <- getToolSetting "Haskell CPP command"
  cpp_args_str <- getSetting "Haskell CPP flags"

  platform <- either error pure $ compatGetTargetPlatform settingsFile mySettings

  let unreg_cc_args = if platformUnregisterised platform
                      then ["-DNO_REGS", "-DUSE_MINIINTERPRETER"]
                      else []
      cpp_args = map Option (words cpp_args_str)
      cc_args  = words cc_args_str ++ unreg_cc_args
      cxx_args = words cxx_args_str
  ldSupportsCompactUnwind <- getBooleanSetting "ld supports compact unwind"
  ldSupportsFilelist      <- getBooleanSetting "ld supports filelist"
  ldIsGnuLd               <- getBooleanSetting "ld is GNU ld"

  let globalpkgdb_path = installed "package.conf.d"
      ghc_usage_msg_path  = installed "ghc-usage.txt"
      ghci_usage_msg_path = installed "ghci-usage.txt"

  -- For all systems, unlit, split, mangle are GHC utilities
  -- architecture-specific stuff is done when building Config.hs
  unlit_path <- getToolSetting "unlit command"

  windres_path <- getToolSetting "windres command"
  ar_path <- getToolSetting "ar command"
  otool_path <- getToolSetting "otool command"
  install_name_tool_path <- getToolSetting "install_name_tool command"
  ranlib_path <- getToolSetting "ranlib command"

  touch_path <- getToolSetting "touch command"

  mkdll_prog <- getToolSetting "dllwrap command"
  let mkdll_args = []

  -- cpp is derived from gcc on all platforms
  -- HACK, see setPgmP below. We keep 'words' here to remember to fix
  -- Config.hs one day.


  -- Other things being equal, as and ld are simply gcc
  cc_link_args_str <- getSetting "C compiler link flags"
  let   as_prog  = cc_prog
        as_args  = map Option cc_args
        ld_prog  = cc_prog
        ld_args  = map Option (cc_args ++ words cc_link_args_str)
  ld_r_prog <- getToolSetting "Merge objects command"
  ld_r_args <- getSetting "Merge objects flags"
  let ld_r
        | null ld_r_prog = Nothing
        | otherwise      = Just (ld_r_prog, map Option $ words ld_r_args)

  -- We just assume on command line
  lc_prog <- getSetting "LLVM llc command"
  lo_prog <- getSetting "LLVM opt command"
  lcc_prog <- getSetting "LLVM clang command"

  let iserv_prog = libexec "ghc-iserv"

  return $ Settings
    { sGhcNameVersion = GhcNameVersion
      { ghcNameVersion_programName = "ghc"
      , ghcNameVersion_projectVersion = cProjectVersion
      }

    , sFileSettings = FileSettings
      { fileSettings_ghcUsagePath   = ghc_usage_msg_path
      , fileSettings_ghciUsagePath  = ghci_usage_msg_path
      , fileSettings_toolDir        = mtool_dir
      , fileSettings_topDir         = top_dir
      , fileSettings_globalPackageDatabase = globalpkgdb_path
      }

    , sToolSettings = ToolSettings
      { toolSettings_ldSupportsCompactUnwind = ldSupportsCompactUnwind
      , toolSettings_ldSupportsFilelist      = ldSupportsFilelist
      , toolSettings_ldIsGnuLd               = ldIsGnuLd
      , toolSettings_ccSupportsNoPie         = gccSupportsNoPie

      , toolSettings_pgm_L   = unlit_path
      , toolSettings_pgm_P   = (cpp_prog, cpp_args)
      , toolSettings_pgm_F   = ""
      , toolSettings_pgm_c   = cc_prog
      , toolSettings_pgm_a   = (as_prog, as_args)
      , toolSettings_pgm_l   = (ld_prog, ld_args)
      , toolSettings_pgm_lm  = ld_r
      , toolSettings_pgm_dll = (mkdll_prog,mkdll_args)
      , toolSettings_pgm_T   = touch_path
      , toolSettings_pgm_windres = windres_path
      , toolSettings_pgm_ar = ar_path
      , toolSettings_pgm_otool = otool_path
      , toolSettings_pgm_install_name_tool = install_name_tool_path
      , toolSettings_pgm_ranlib = ranlib_path
      , toolSettings_pgm_lo  = (lo_prog,[])
      , toolSettings_pgm_lc  = (lc_prog,[])
      , toolSettings_pgm_lcc = (lcc_prog,[])
      , toolSettings_pgm_i   = iserv_prog
      , toolSettings_opt_L       = []
      , toolSettings_opt_P       = []
      , toolSettings_opt_P_fingerprint = fingerprint0
      , toolSettings_opt_F       = []
      , toolSettings_opt_c       = cc_args
      , toolSettings_opt_cxx     = cxx_args
      , toolSettings_opt_a       = []
      , toolSettings_opt_l       = []
      , toolSettings_opt_lm      = []
      , toolSettings_opt_windres = []
      , toolSettings_opt_lcc     = []
      , toolSettings_opt_lo      = []
      , toolSettings_opt_lc      = []
      , toolSettings_opt_i       = []

      , toolSettings_extraGccViaCFlags = []
      }

    , sTargetPlatform = platform

    -- Lots of uninitialized fields here.
    , sPlatformMisc = PlatformMisc {}

    , sRawSettings    = settingsList
    }

-- Stripped version of 'GHC.Settings.Platform.getTargetPlatform'. Arch info is
-- needed for CPP defines, the rest is irrelevant.
compatGetTargetPlatform
  :: FilePath -> RawSettings -> Either String Platform
compatGetTargetPlatform settingsFile mySettings = do
  let
    readSetting :: (Show a, Read a) => String -> Either String a
    readSetting = readRawSetting settingsFile mySettings

  targetArchOS <- getTargetArchOS settingsFile mySettings
  targetWordSize <- readSetting "target word size"

  pure $ Platform
    { platformArchOS = targetArchOS
    , platformWordSize = targetWordSize
    , platform_constants = Nothing
    -- below is irrelevant
    , platformByteOrder = LittleEndian
    , platformUnregisterised = True
    , platformHasGnuNonexecStack = False
    , platformHasIdentDirective = False
    , platformHasSubsectionsViaSymbols = False
    , platformIsCrossCompiling = False
    , platformLeadingUnderscore = False
    , platformTablesNextToCode  = False
    , platformHasLibm = False
    }

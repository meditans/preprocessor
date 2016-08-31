{-# LANGUAGE CPP, RecordWildCards #-}

{-|
Module      : Preprocessor.Internal.Preprocess
Description : Call GHC.preprocess at the Cpp phase
Copyright   : (c) Carlo Nucera, 2016
License     : BSD3
Maintainer  : meditans@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Preprocessor.Internal.Preprocess
   (
   -- * Entry point for parsing
     parseModuleWithCpp
   -- * Wrapper around GHC's preprocess
   , getPreprocessedSrcDirect
   ) where

import Control.Monad               (void)
import Data.Tuple.Extra            (fst3)
import Lens.Micro
import Preprocessor.Internal.Types

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import qualified DriverPhases   as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags       as GHC
import qualified GHC
import qualified GHC.Paths      as GHC
import qualified HeaderInfo     as GHC
import qualified HscTypes       as GHC
import qualified MonadUtils     as GHC

# if __GLASGOW_HASKELL__ >= 800
import qualified Language.Haskell.TH.LanguageExtensions as LE
# endif

--------------------------------------------------------------------------------
-- Entry point for parsing
--------------------------------------------------------------------------------

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp :: CppOptions -> FilePath -> IO String
parseModuleWithCpp cppOptions file = GHC.runGhc (Just GHC.libdir) $ do
  dflags <- initDynFlags file
#if __GLASGOW_HASKELL__ >= 800
  let useCpp = GHC.xopt LE.Cpp dflags
#else
  let useCpp = GHC.xopt GHC.Opt_Cpp dflags
#endif
  if useCpp
    then getPreprocessedSrcDirect cppOptions file
    else GHC.liftIO (readFile file)

-- | Given a config and a file, this function returns the correct dynFlags. For
-- now, this is only used to check if the Cpp extension is enabled (see the test
-- in parseModuleWithCpp), and thus it could be implemented differently. I'm
-- keeping this version to have all the flags, in case I decide later to offer
-- an entry point for ghc-api based analysis.
initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0  <- GHC.getSessionDynFlags
  src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags0 file
  dflags1  <- fst3 <$> GHC.parseDynamicFilePragma dflags0 src_opts
  let dflags2 = dflags1 {GHC.log_action = GHC.defaultLogAction}
  void $ GHC.setSessionDynFlags dflags2
  return dflags2

--------------------------------------------------------------------------------
-- Wrapper around GHC's preprocess
--------------------------------------------------------------------------------

-- | Invoke GHC's 'GHC.preprocess' function at the cpp phase, adding the
-- options specified in the first argument.
getPreprocessedSrcDirect :: GHC.GhcMonad m => CppOptions -> FilePath -> m String
getPreprocessedSrcDirect cppOptions file = do
  hscEnv <- injectCppOptions cppOptions <$> GHC.getSession
  (_, tempFile) <- GHC.liftIO $ GHC.preprocess hscEnv (file, cppPhase)
  GHC.liftIO (readFile tempFile)
  where
    cppPhase = Just (GHC.Cpp GHC.HsSrcFile)

injectCppOptions :: CppOptions -> GHC.HscEnv -> GHC.HscEnv
injectCppOptions CppOptions{..} = over (_hsc_dflags . _settings . _sOpt_P)
                                       (encodedOptions ++)
  where
    encodedOptions = map ("-D" ++) cppDefine
                  ++ map ("-I" ++) cppInclude
                  ++ map ("-include" ++) cppFile

--------------------------------------------------------------------------------
-- Some lenses to manipulate conveniently a HscEnv value
--------------------------------------------------------------------------------

_hsc_dflags :: Lens' GHC.HscEnv GHC.DynFlags
_hsc_dflags = lens GHC.hsc_dflags
                   (\hscEnv dynFlags -> hscEnv {GHC.hsc_dflags = dynFlags})

_settings :: Lens' GHC.DynFlags GHC.Settings
_settings = lens GHC.settings
                 (\dynFlags setting -> dynFlags {GHC.settings = setting})

_sOpt_P :: Lens' GHC.Settings [String]
_sOpt_P = lens GHC.sOpt_P
               (\setting strings -> setting {GHC.sOpt_P = strings})

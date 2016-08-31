{-# LANGUAGE CPP, RecordWildCards #-}

{-|
Module      : Preprocessor.Internal.Preprocess
Description : Call GHC.preprocess at the Cpp phase
Copyright   : (c) Carlo Nucera, 2016
License     : BSD3
Maintainer  : meditans@gmail.com
Stability   : experimental
Portability : POSIX

The main function exported by this module, 'getPreprocessedSrcDirect', is a
wrapper around Ghc's 'GHC.preprocess'.
-}

module Preprocessor.Internal.Preprocess
   ( CppOptions(..)
   , emptyCppOptions
   , getPreprocessedSrcDirect
   ) where

import Lens.Micro
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import qualified DriverPhases   as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags       as GHC
import qualified GHC
import qualified HscTypes       as GHC
import qualified MonadUtils     as GHC

--------------------------------------------------------------------------------
-- Data types declaration
--------------------------------------------------------------------------------

-- | `CppOptions` represent the options which are passed, through the ghc api,
-- to the cpp preprocessing program. For reference,
-- <https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html here> is the
-- part of the gcc manual corresponding to the preprocessing options.
data CppOptions = CppOptions
                { cppDefine  :: [String]
                -- ^ CPP #define macros. Corresponds to a @-D@ option for the
                -- cpp program.
                , cppInclude :: [FilePath]
                -- ^ CPP Includes directory. Corresponds to a @-I@ option for
                -- the cpp program.
                , cppFile    :: [FilePath]
                -- ^ CPP pre-include file. Corresponds to a @-include@ option
                -- for the cpp program.
                } deriving (Show)

-- |
-- >>> emptyCppOptions
-- CppOptions {cppDefine = [], cppInclude = [], cppFile = []}
emptyCppOptions :: CppOptions
emptyCppOptions = CppOptions [] [] []

--------------------------------------------------------------------------------
-- Main functions
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

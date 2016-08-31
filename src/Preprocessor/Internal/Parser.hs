{-# LANGUAGE CPP #-}

{-|
Module      : Preprocessor.Internal.Parser
Description : Initializes the flags and calls getPreprocessedSrcDirect
Copyright   : (c) Carlo Nucera, 2016
License     : BSD3
Maintainer  : meditans@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Preprocessor.Internal.Parser (parseModuleWithCpp) where

import Control.Monad           (void)
import Data.Tuple.Extra        (fst3)
import Preprocessor.Internal.Preprocess
import Preprocessor.Internal.Types

import qualified DynFlags   as GHC
import qualified GHC        hiding (parseModule)
import qualified GHC.Paths  as GHC
import qualified HeaderInfo as GHC
import qualified MonadUtils as GHC

# if __GLASGOW_HASKELL__ >= 800
import qualified Language.Haskell.TH.LanguageExtensions as LE
# endif

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

-- Given a config and a file, this function returns the correct dynFlags. For
-- now, this is only used to check if the Cpp extension is enabled (see the test
-- in parseModuleWithCpp), and thus it could be implemented differently. I'm
-- keeping this version to have all the flags, in case I decide later to offer
-- an entry point for ghc-api based analysis.
initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
    dflags0 <- GHC.getSessionDynFlags
    src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags0 file
    dflags1 <- fst3 <$> GHC.parseDynamicFilePragma dflags0 src_opts
    let dflags2 = dflags1 { GHC.log_action = GHC.defaultLogAction }
    void $ GHC.setSessionDynFlags dflags2
    return dflags2
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Preprocessor.Parser (parseModule) where

import Control.Monad (void)

import qualified GHC hiding (parseModule)
import qualified DynFlags     as GHC
import qualified HeaderInfo   as GHC
import qualified MonadUtils   as GHC
import qualified GHC.Paths  as GHC

# if __GLASGOW_HASKELL__ >= 800
import qualified Language.Haskell.TH.LanguageExtensions as LE
# endif

import Preprocessor.Preprocess
import Preprocessor.Types

-- | Parse a module with the default instructions for the C pre-processor Only
--   the includes directory is taken from the config. This is the modified
--   version which returns the string of the parsed file.
parseModule :: Config -> FilePath -> IO String
parseModule conf = parseModuleWithCpp conf $
    emptyCppOptions { cppInclude = includeDirs conf
                    , cppFile    = headers conf
                    }

-- | Parse a module with specific instructions for the C pre-processor. This is
-- a modified version, which exports the string of the file, without parsing it
-- with the ghc api.
parseModuleWithCpp :: Config -> CppOptions -> FilePath -> IO String
parseModuleWithCpp conf cppOptions file = GHC.runGhc (Just GHC.libdir) $ do
  dflags <- initDynFlags conf file
#if __GLASGOW_HASKELL__ >= 800
  let useCpp = GHC.xopt LE.Cpp dflags
#else
  let useCpp = GHC.xopt GHC.Opt_Cpp dflags
#endif
  fileContents <- if useCpp
                  then getPreprocessedSrcDirect cppOptions file
                  else GHC.liftIO (readFile file)
  return fileContents

initDynFlags :: GHC.GhcMonad m => Config -> FilePath -> m GHC.DynFlags
initDynFlags conf file = do
    dflags0 <- GHC.getSessionDynFlags
    (dflags1,_,_) <- GHC.parseDynamicFlagsCmdLine dflags0
        [GHC.L GHC.noSrcSpan ("-X" ++ e) | e <- exts conf]
    src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
    (dflags2, _, _) <- GHC.parseDynamicFilePragma dflags1 src_opts
    let dflags3 = dflags2 { GHC.log_action = GHC.defaultLogAction }
    void $ GHC.setSessionDynFlags dflags3
    return dflags3

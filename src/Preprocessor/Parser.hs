{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Preprocessor.Parser (parseModule) where

import Control.Monad (void)
import qualified Control.Exception as E

import qualified GHC hiding (parseModule)
import qualified DynFlags     as GHC
import qualified HeaderInfo   as GHC
import qualified MonadUtils   as GHC
import qualified Outputable   as GHC
import GHC.Paths (libdir)

# if __GLASGOW_HASKELL__ >= 800
import qualified Language.Haskell.TH.LanguageExtensions as LE
# endif

import Preprocessor.Preprocess
import Preprocessor.Types
import Preprocessor.Loc

-- | Parse a module with the default instructions for the C pre-processor Only
--   the includes directory is taken from the config. This is the modified
--   version which returns the string of the parsed file.
parseModule :: Config -> FilePath -> IO String
parseModule conf = parseModuleWithCpp conf $
    defaultCppOptions { cppInclude = includeDirs conf
                      , cppFile    = headers conf
                      }

-- | Parse a module with specific instructions for the C pre-processor. This is
-- a modified version, which exports the string of the file, without parsing it
-- with the ghc api.
parseModuleWithCpp :: Config
                   -> CppOptions
                   -> FilePath
                   -> IO String
parseModuleWithCpp conf cppOptions file =
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags conf file
#if __GLASGOW_HASKELL__ >= 800
      let useCpp = GHC.xopt LE.Cpp dflags
#else
      let useCpp = GHC.xopt GHC.Opt_Cpp dflags
#endif
      (fileContents, _) <-
        if useCpp
           then getPreprocessedSrcDirect cppOptions file
           else do
               contents <- GHC.liftIO $ readFile file
               return (contents, dflags)
      return $ fileContents

initDynFlags :: GHC.GhcMonad m => Config -> FilePath -> m GHC.DynFlags
initDynFlags conf file = do
    dflags0 <- GHC.getSessionDynFlags
    (dflags1,_,_) <- GHC.parseDynamicFlagsCmdLine dflags0
        [GHC.L GHC.noSrcSpan ("-X" ++ e) | e <- exts conf]
    src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
    (dflags2, _, _) <- GHC.parseDynamicFilePragma dflags1 src_opts
    let dflags3 = dflags2 { GHC.log_action = customLogAction }
    void $ GHC.setSessionDynFlags dflags3
    return dflags3

customLogAction :: GHC.LogAction
#if __GLASGOW_HASKELL__ >= 800
customLogAction dflags _ severity srcSpan _ m =
#else
customLogAction dflags severity srcSpan _ m =
#endif
    case severity of
      GHC.SevFatal -> throwError
      GHC.SevError -> throwError
      _            -> return ()
    where throwError = E.throwIO $ GhcParseError (srcSpanToLoc srcSpan)
                                                 (GHC.showSDoc dflags m)

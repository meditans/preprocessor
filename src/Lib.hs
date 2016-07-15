-- * Descrizione della libreria

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Lib where

import Preprocessor.Preprocess
import GHC
import GHC.Paths (libdir)
import Control.Monad (void, (>=>))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription
       (condTreeData, condLibrary, hsSourceDirs, exposedModules,
        libBuildInfo)
import Distribution.ModuleName (toFilePath)
import Distribution.Verbosity (silent)

import System.FilePath.Posix
import System.Process (readProcess)
import Control.Monad.Trans

-- | This is intended as the primary entry point for this application. Call this
-- function to preprocess the files.

preprocessSourceFile :: FilePath -> FilePath -> IO String
preprocessSourceFile cabalPath fp = fmap fst . runGhc (Just libdir) $ do
  macros <- liftIO (findCabalMacros cabalPath)
  dflags <- getSessionDynFlags
  void . setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                     , ghcLink   = LinkInMemory }
  getPreprocessedSrcDirect defaultCppOptions{cppInclude = [macros]} fp

-- Given the cabal file, this returns all the exposed modules of the library, if
-- this is a library.
getExposedModulesPath :: FilePath -> IO [FilePath]
getExposedModulesPath cabalPath = do
  packageDesc <- readPackageDescription silent cabalPath
  let Just lib = condTreeData <$> condLibrary packageDesc
      modules = map toFilePath . exposedModules $ lib
      (srcDir:_) = hsSourceDirs $ libBuildInfo lib
  return $ map (\p -> takeDirectory cabalPath </> srcDir </> p <.> "hs") modules

exCabalFilePath, lensCabalFilePath :: String
exCabalFilePath = "/home/carlo/code/haskell/preprocessor/preprocessor.cabal"
lensCabalFilePath = "/home/carlo/code/haskell/forks/lens-4.14/lens.cabal"

tryAllFiles :: FilePath -> IO ()
tryAllFiles cabalPath = do
  ms <- getExposedModulesPath cabalPath
  mapM_ (preprocessSourceFile cabalPath >=> putStrLn) ms

findCabalMacros :: FilePath -> IO String
findCabalMacros cabalPath = do
  relPath <- init <$> readProcess "stack" ["path", "--dist-dir"] ""
  let absPath = takeDirectory cabalPath
  return $ absPath </> relPath


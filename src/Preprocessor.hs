{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Preprocessor (getExposedModulesPath, preprocessFile) where

import           Distribution.ModuleName               (toFilePath)
import           Distribution.PackageDescription       (condLibrary,
                                                        condTreeData,
                                                        exposedModules,
                                                        hsSourceDirs,
                                                        libBuildInfo)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (silent)

import           Control.Monad                         (filterM, (>=>))
import           Data.List                             (inits, isPrefixOf,
                                                        isSuffixOf)
import           Data.Monoid                           ((<>))
import           Preprocessor.Parser                   (parseModule)
import           Preprocessor.Types
import           System.Directory.Extra                (listContents)
import           System.FilePath.Posix
import           System.Process                        (readCreateProcess,
                                                        readProcess, shell)

-- | Given the cabal file, this returns all the exposed modules of the library,
-- if this is a library. This is one of the two main functions of the modules.
getExposedModulesPath :: FilePath -> IO [FilePath]
getExposedModulesPath cabalPath = do
  packageDesc <- readPackageDescription silent cabalPath
  let Just lib = condTreeData <$> condLibrary packageDesc
      modules = map toFilePath . exposedModules $ lib
      (srcDir:_) = hsSourceDirs $ libBuildInfo lib
  return $ map (\p -> takeDirectory cabalPath </> srcDir </> p <.> "hs") modules

-- | This function finds the location of the cabal macros given the cabal file.
-- It's currently unused.
findCabalMacros :: FilePath -> IO String
findCabalMacros cabalPath = do
  relPath <- init <$> readProcess "stack" ["path", "--dist-dir"] ""
  let absPath = takeDirectory cabalPath
  return $ absPath </> relPath

-- | This is intended as the main function of the library. There is currently a
-- workaround in removing all the lines that begin with #
preprocessFile :: FilePath -> IO String
preprocessFile fp = do
  macroFile <- fromGenericFileToCppMacroFile fp
  rawString <- parseModule (defaultConfig {headers = [macroFile]}) fp
  return . unlines . filter (not . isPrefixOf "#") . lines $ rawString

-- | This is the important function; from a file, it generates the cabal macro file to use.
fromGenericFileToCppMacroFile :: FilePath -> IO FilePath
fromGenericFileToCppMacroFile fp = do
  distDir <- (findProjectDirectory >=> findDistDir) fp
  return $ distDir <> "/build/autogen/cabal_macros.h"

-- | The project directory is the one that contains the .cabal file. Takes a
-- filepath of a file in the project, and traverses the structure until it
-- founds the .stack-work as a subdirectory.
findProjectDirectory :: FilePath -> IO FilePath
findProjectDirectory fileInProject = do
  let splittedPath  = splitPath fileInProject
      possiblePaths = map joinPath $ init $ tail $ inits splittedPath
  head <$> filterM containsStackWork possiblePaths
 where
   containsStackWork :: FilePath -> IO Bool
   containsStackWork dir = do
     ds <- listContents dir
     return $ any (".stack-work" `isSuffixOf`) ds

-- | Given a directory (which is meant to be the project directory), gives back
-- the dist-dir contained in it (it's important because it contains all the
-- macros).
findDistDir :: FilePath -> IO FilePath
findDistDir fp = init <$> readCreateProcess (shell cmd) ""
  where cmd = "cd " ++ fp ++ "; " ++ "cd $(stack path --dist-dir)" ++ "; pwd"

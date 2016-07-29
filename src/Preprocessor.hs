{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Preprocessor (getLibExposedModulesPath, preprocessFile) where

import Control.Monad                         (filterM, (>=>))
import Data.List                             (inits, isPrefixOf, isSuffixOf)
import Data.Maybe                            (catMaybes, isNothing)
import Data.Monoid                           ((<>))
import Distribution.ModuleName               (toFilePath)
import Distribution.PackageDescription
    (condLibrary, condTreeData, exposedModules, hsSourceDirs, libBuildInfo)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import Preprocessor.Parser                   (parseModule)
import Preprocessor.Types
import System.Directory                      (findFile, makeAbsolute)
import System.Directory.Extra                (listContents)
import System.FilePath.Find
import System.FilePath.Posix
import System.Process                        (readCreateProcess, shell)

-- | ProjectDir is the directory which contains the .cabal file for the project
type ProjectDir    = FilePath
type CabalFilePath = FilePath

-- | Given the cabal file, this returns all the exposed modules of the library,
-- if this is a library. This is one of the two main functions of the module.
getLibExposedModulesPath :: CabalFilePath -> IO [FilePath]
getLibExposedModulesPath cabalPath = do
  packageDesc <- readPackageDescription silent cabalPath
  let Just lib = condTreeData <$> condLibrary packageDesc
      modules = map (++".hs") . map toFilePath . exposedModules $ lib
      sourceDirs = map (takeDirectory cabalPath </>) . ("" :) . hsSourceDirs $ libBuildInfo lib
  mbModulesPath <- mapM (findFile sourceDirs) modules
  if any isNothing mbModulesPath
    then print "Error in exposed modules retrieving"
    else return ()
  return $ catMaybes mbModulesPath

-- | This is intended as the main function of the library. There is currently a
-- workaround in removing all the lines that begin with #
preprocessFile :: FilePath -> IO String
preprocessFile fp = do
  projectDir   <- findProjectDirectory fp
  macroFile    <- fromGenericFileToCppMacroFile fp
  includeFiles <- allHFiles projectDir >>= mapM (\x -> takeDirectory <$> makeAbsolute x)
  rawString    <- parseModule (defaultConfig {headers = [macroFile], includeDirs = includeFiles}) fp
  let filteredString = unlines . filter (not . isPrefixOf "#") . lines $ rawString
  return filteredString

-- | This is the important function; from a file, it generates the cabal macro file to use.
fromGenericFileToCppMacroFile :: FilePath -> IO FilePath
fromGenericFileToCppMacroFile fp = do
  distDir <- (findProjectDirectory >=> findDistDir) fp
  return $ distDir <> "/build/autogen/cabal_macros.h"

-- | The project directory is the one that contains the .cabal file. Takes a
-- filepath of a file in the project, and traverses the structure until it
-- founds the directory containing the .cabal file.
findProjectDirectory :: FilePath -> IO ProjectDir
findProjectDirectory fileInProject = do
  let splittedPath  = splitPath fileInProject
      possiblePaths = map joinPath $ init $ tail $ inits splittedPath
  head <$> filterM containsCabalFile possiblePaths
 where
   containsCabalFile :: FilePath -> IO Bool
   containsCabalFile dir = any (".cabal" `isSuffixOf`) <$> listContents dir

-- | Given a directory (which is meant to be the project directory), gives back
-- the dist-dir contained in it (it's important because it contains all the
-- macros).
findDistDir :: ProjectDir -> IO FilePath
findDistDir fp = init <$> readCreateProcess (shell cmd) ""
  where cmd = "cd " ++ fp ++ "; " ++ "cd $(stack path --dist-dir)" ++ "; pwd"

-- | Given the project directory, tries to find additional .h files (which could
-- be additional macros). For now, it just finds, recursively, every .h file
-- which is not cabal-macros.h
allHFiles :: ProjectDir -> IO [FilePath]
allHFiles root = find always (extension ==? ".h" &&? fileName /=? "cabal_macros.h") root

{-# LANGUAGE ViewPatterns #-}

module Preprocessor (getLibExposedModulesPath, preprocessFile) where

import Control.Monad                         (filterM, (>=>))
import Data.List                             (inits, isSuffixOf)
import Data.Maybe                            (catMaybes)
import Data.Monoid                           ((<>))
import Distribution.ModuleName               (toFilePath)
import Distribution.PackageDescription       (condLibrary, condTreeData,
                                             exposedModules, hsSourceDirs,
                                             libBuildInfo)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import Preprocessor.Parser                   (parseModuleWithCpp)
import Preprocessor.Types                    (CabalFilePath, ProjectDir)
import Preprocessor.CppOutput
import Preprocessor.Preprocess
import System.Directory                      (findFile, makeAbsolute)
import System.Directory.Extra                (listContents)
import System.FilePath.Find                  (always, extension, fileName, find,
                                             (&&?), (/=?), (==?))
import System.FilePath.Posix                 (joinPath, splitPath,
                                             takeDirectory, (</>))
import System.Process                        (readCreateProcess, shell)

-- | Given the cabal file, this returns all the exposed modules of the library,
-- if this is a library. This is one of the two main functions of the module.
getLibExposedModulesPath :: CabalFilePath -> IO [FilePath]
getLibExposedModulesPath cabalPath = do
  packageDesc <- readPackageDescription silent cabalPath
  let Just lib = condTreeData <$> condLibrary packageDesc
      modules = map (++".hs") . map toFilePath . exposedModules $ lib
      sourceDirs = map (takeDirectory cabalPath </>) . ("" :) . hsSourceDirs $ libBuildInfo lib
  mbModulesPath <- mapM (findFile sourceDirs) modules
  return (catMaybes mbModulesPath)

-- | This is intended as the main function of the library. There is currently a
-- workaround in removing all the lines that begin with #
preprocessFile :: FilePath -> IO String
preprocessFile fp = do
  projectDir   <- findProjectDirectory fp
  macroFile    <- fromGenericFileToCppMacroFile fp
  includeFiles <- allDotHFiles projectDir >>= mapM (\x -> takeDirectory <$> makeAbsolute x)
  rawString    <- parseModuleWithCpp (emptyCppOptions { cppFile    = [macroFile]
                                               , cppInclude = includeFiles }) fp
  return . unlines
         . reconstructSource
         . discardUnusefulComponents fp
         . parseCppOutputComponents
         . lines $ rawString

--------------------------------------------------------------------------------
-- Functions to locate the various paths
--------------------------------------------------------------------------------

-- | From a file position, it locates the cabal macro file (cabal_macros.h) to
-- use.
fromGenericFileToCppMacroFile :: FilePath -> IO FilePath
fromGenericFileToCppMacroFile fp = do
  distDir <- (findProjectDirectory >=> findDistDir) fp
  return $ distDir <> "/build/autogen/cabal_macros.h"

-- | The project directory is the one that contains the .cabal file. Takes a
-- filepath of a file in the project.
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

-- | Given the project directory, finds the directories in which additional .h
-- files (which could be additional macros) are stored.
allDotHFiles :: ProjectDir -> IO [FilePath]
allDotHFiles root = find always isDotH root
  where
    isDotH = (extension ==? ".h" &&? fileName /=? "cabal_macros.h")

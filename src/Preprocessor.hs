{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Preprocessor
Description : Preprocess the cpp of a haskell source to do static analysis
Copyright   : (c) Carlo Nucera, 2016
License     : BSD3
Maintainer  : meditans@gmail.com
Stability   : experimental
Portability : POSIX

Preprocessor preprocesses the cpp in the source of a haskell library (a task not
usually done by parsing libraries) to prepare for a static analysis of the code,
e.g. with
<http://hackage.haskell.org/package/haskell-src-exts haskell-src-exts>.

The design of the library is guided by two principles:

  * Line numbering with the original file should be preserved: if a line isn't
related to cpp preprocessing, it conserves its position. This is done to make
eventual failings with the parsing library easier to locate.

  * It should offer a very simple API, shielding the user from the understandings
of how cabal options are passed around, and trying to automatically find all the
required information in the project. The user is expected to use only the two
functions in this module.

Currently this tool __requires__ the library to have been built with stack (it
searches for some files generated in @.stack-work@). In the future I'll
probably lift this restriction (if you need it before, please open a ticket).
The files marked as internal are exported for documentation purposes only.
-}
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
import Preprocessor.Internal.AddPadding
import Preprocessor.Internal.Preprocess      (parseModuleWithCpp)
import Preprocessor.Internal.Types           (CabalFilePath, CppOptions (..),
                                             ProjectDir, emptyCppOptions)
import System.Directory                      (findFile, makeAbsolute)
import System.Directory.Extra                (listContents)
import System.FilePath.Find                  (always, extension, fileName, find,
                                             (&&?), (/=?), (==?))
import System.FilePath.Posix                 (joinPath, splitPath,
                                             takeDirectory, (</>))
import System.Process                        (readCreateProcess, shell)

-- | Given the path to the cabal file, this returns the paths to all the exposed
-- modules of the @library@ section.
getLibExposedModulesPath :: CabalFilePath -> IO [FilePath]
getLibExposedModulesPath cabalPath = do
  packageDesc <- readPackageDescription silent cabalPath
  let Just lib = condTreeData <$> condLibrary packageDesc
      modules = map (++".hs") . map toFilePath . exposedModules $ lib
      sourceDirs = map (takeDirectory cabalPath </>) . ("" :) . hsSourceDirs $ libBuildInfo lib
  mbModulesPath <- mapM (findFile sourceDirs) modules
  return (catMaybes mbModulesPath)

-- | Given the path to a file in a stack-build project, returns the content of
-- the preprocessed file. The line numbering of the original file is preserved.
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

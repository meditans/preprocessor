{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Preprocessor (getLibExposedModulesPath, preprocessFile, CabalFilePath) where

import Control.Monad                         (filterM, (>=>))
import Data.Char                             (isDigit)
import Data.List                             (inits, isSuffixOf)
import Data.List.Extra                       (repeatedly)
import Data.Maybe                            (catMaybes, isNothing)
import Data.Monoid                           ((<>))
import Distribution.ModuleName               (toFilePath)
import Distribution.PackageDescription       (condLibrary, condTreeData,
                                             exposedModules, hsSourceDirs,
                                             libBuildInfo)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import Preprocessor.Parser                   (parseModule)
import Preprocessor.Types
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
  rawString    <- parseModule (defaultConfig { headers     = [macroFile]
                                             , includeDirs = includeFiles}) fp
  return . unlines
         . reconstructSource
         . discardUnusefulComponents fp
         . parseCppOutputComponents
         . lines $ rawString

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

-- Given a string (representing file content) littered with cpp annotations,
-- discards the one not pertinent with the file.
-- Cue https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

-- | A linemarker follows the structure described in
-- https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
data LineMarker = LineMarker { beginsAt :: Int
                             , filePath :: FilePath
                             } deriving (Show)

-- | A CppOutputComponent represents a linemarker and the block of code till the
-- next linemarker.
data CppOutputComponent = CppOutputComponent { lineMarker  :: LineMarker
                                             , sourceBlock :: [String]
                                             } deriving (Show)

-- | A linemarker begins with #, which is not a valid way to begin a line in
-- haskell.
isLineMarker :: String -> Bool
isLineMarker (words -> w:_) = w == "#"
isLineMarker _              = False

-- | Tries to parse a linemaker. This function is brittle in this iteration.
parseLineMarker :: String -> LineMarker
parseLineMarker s =
  if all isDigit (words s !! 1)
  then LineMarker (read $ words s !! 1) (unquote $ words s !! 2)
  else error $ "I didn't correctly parse " ++ s ++ "as a cpp linemarker"
  where unquote = tail . init

-- | Given the lines of a file, extracts the Cpp components. Note that a file
-- that doesn't need cpp preprocessing doesn't have any linemarker, so we must
-- detect that case and handle it appropriately.
parseCppOutputComponents :: [String] -> [CppOutputComponent]
parseCppOutputComponents ss =
  if any isLineMarker ss
  then repeatedly (\ss' -> let (content, rest) = span (not . isLineMarker) (tail ss')
                           in (CppOutputComponent (parseLineMarker $ head ss') content , rest)) ss
  else [CppOutputComponent (LineMarker 1 "") ss]

-- | Discard the parts of cpp output which derive from recursively parsed
-- modules.
discardUnusefulComponents :: FilePath -> [CppOutputComponent] -> [CppOutputComponent]
discardUnusefulComponents _ [] =
  error "The function discardUnusefulComponents expects a non-empty list of components"
discardUnusefulComponents _ (c:[]) = [c]
discardUnusefulComponents fp cs =
  filter (\c -> filePath (lineMarker c) /= fp) (tail cs)

-- | Adds padding to the source blocks to mantain the correct line numbers of
-- the source code.
reconstructSource :: [CppOutputComponent] -> [String]
reconstructSource = sourceBlock . foldr1 combine
  where
    combine (CppOutputComponent lm1 c1) (CppOutputComponent lm2 c2) =
      CppOutputComponent lm1 (c1 ++ replicate (beginsAt lm2 - beginsAt lm1 - length c1) "" ++ c2)

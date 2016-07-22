-- * Descrizione dell'applicazione
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import Lib
import Preprocessor.Parser (parseModule)
import Preprocessor.Types

import System.FilePath.Posix
import System.Process
import Data.Monoid
import System.Directory.Extra
import Data.List
import Control.Monad


-- | This is intended as the main function of the library. There is currently a
-- workaround in removing all the lines that begin with #
getPurifiedSource :: FilePath -> IO String
getPurifiedSource fp = do
  macroFile <- fromGenericFileToCppMacroFile fp
  rawString <- parseModule (defaultConfig {headers = [macroFile]}) fp
  return . unlines . filter (not . isPrefixOf "#") . lines $ rawString

-- | The project directory is the one that contains the .cabal file. Takes a
-- filepath of a file in the project, and traverses the structure until it
-- founds the .stack-work

findProjectDirectory :: FilePath -> IO FilePath
findProjectDirectory fileInProject = do
  let splittedPath = splitPath fileInProject
  let possiblePaths = map joinPath $ init $ tail $ inits splittedPath
  head <$> filterM containsStackWork possiblePaths

containsStackWork :: FilePath -> IO Bool
containsStackWork dir = do
  ds <- listContents dir
  return $ any (".stack-work" `isSuffixOf`) ds

-- | Takes the directory in which executes the command
findDistDir :: FilePath -> IO FilePath
findDistDir fp = init <$> readCreateProcess (shell cmd) ""
  where cmd = "cd " ++ fp ++ "; " ++ "cd $(stack path --dist-dir)" ++ "; pwd"

-- | This is the important function; from a file, it generates the cabal macro file to use.
fromGenericFileToCppMacroFile :: FilePath -> IO FilePath
fromGenericFileToCppMacroFile fp = do
  distDir <- (findProjectDirectory >=> findDistDir) fp
  return $ distDir <> "/build/autogen/cabal_macros.h"

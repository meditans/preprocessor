{-# LANGUAGE PartialTypeSignatures #-}
module LibSpec where

import Test.Hspec

import Preprocessor

import Control.Monad    (void, when)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath  (takeFileName)
import System.IO.Temp   (withSystemTempDirectory)
import System.Process   (readCreateProcessWithExitCode, shell)
import Test.Utils       (withCurrentDirectory)

spec :: Spec
spec = do
  describe "tautologies" $ do
    it "True is True" $ do
      True `shouldBe` True

-- The eventual purpose of this test suite is to ensure that all the exposed
-- files of the most important libraries are parsed correctly in a state which
-- is compatible with haskell-src-exts.

-- | WIP: This is the entry point if the test suite wants to install the
-- packages by itself.
withStackInstalledPackage :: IO ()
withStackInstalledPackage =
  withSystemTempDirectory "preprocessorTest" $
  \dir -> withCurrentDirectory dir $ do
    execCmd "stack unpack lens"
    dirPath <- uniqueOutputOf "ls"
    setCurrentDirectory dirPath
    execCmd "stack init --omit-packages"
    execCmd "stack solver"
    execCmd "stack build"

-- | Convenience function to retrieve the result of a command, given it's a
-- single line.
uniqueOutputOf :: String -> IO String
uniqueOutputOf cmd = do
  (_, stdout, _) <- readCreateProcessWithExitCode (shell cmd) ""
  return . head . lines $ stdout

execCmd :: String -> IO ()
execCmd cmd = void $ readCreateProcessWithExitCode (shell cmd) ""

-- | WIP Given a path to the cabal file, tries to parse every file with haskell-src-exts
analyze :: CabalFilePath -> IO ()
analyze cabalPath = do
  modulePaths   <- getLibExposedModulesPath cabalPath
  parsedModules <- mapM preprocessFile modulePaths
  mapM_ putStrLn parsedModules

-- | Check that, if a line starts with # in the original file, it has been
-- substituted with a blank line during the parsing.
sameContent :: FilePath -> IO ()
sameContent fp = do
  rawFile    <- readFile fp
  parsedFile <- preprocessFile fp
  let test = not . and $ zipWith compatible (lines rawFile) (lines parsedFile)
  when test $ do
    putStrLn fp
    createDirectoryIfMissing True "./tmp"
    writeFile ("tmp/Raw"    ++ takeFileName fp) rawFile
    writeFile ("tmp/Parsed" ++ takeFileName fp) parsedFile
  where
    compatible ('#':_) "" = True
    compatible ('#':_) _  = False
    compatible _       _  = True

-- Here are some tests which don't work: checking that every line not starting
-- with # is preserved (cpp may rightfully alter the lines, like it does in
-- lens). The files don't have in general the same number of lines, because the
-- file is not padded after the end of the source.

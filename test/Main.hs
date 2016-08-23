module Main where

import Language.Haskell.Exts
import Preprocessor
import Preprocessor.Types (CabalFilePath)
import Test.Hspec

import Control.Exception (bracket)
import Control.Monad     (void)
import Data.List         ((\\))
import System.Directory  (getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp    (withSystemTempDirectory)
import System.Process    (readCreateProcessWithExitCode, shell)

{- Note [Structure of the test suite]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are checking two properties for each libraries:

- every line which begins with # in the original file is either unchanged (maybe
  it's part of a comment or drawing, like in pipes) or has been substituted in
  the new file with a blank line. The relevant function is hashesAreProcessed.
- after preprocessing, the files are parsed without errors by haskell-src-exts
  (which can't parse cpp by itself). The relevant function is
  isParsedByHaskellSrcExts.

For comparison, here are some tests which don't work naively:

- checking that every line not starting with # is preserved (cpp may rightfully
  alter the lines, like it does in the lens library).
- checking that the files have the same number of lines: this may fail because
  the file is not padded after the end of the source.

These tests could be adapted, for example setting a limit on the edit distance
for the first one, or manually padding the end of the file for the second one.
For the moment I'll keep the simple tests I outlined earlier.
-}

-- Check some important libraries. I still have a problem in processing "text",
-- due to the enabling of some flags in the cabal file.
main :: IO ()
main = mapM_ testPackage testLibraries
  where
    testLibraries = [ "lens", "varying", "aeson", "pipes"
                    , "conduit", "wreq", "haskell-src-exts" ]

-- Given the name of the package, builds it in a temporary location and executes
-- the tests.
testPackage :: String -> IO ()
testPackage packageName =
  withStackInstalledPackage packageName $ \cabalPath -> hspec $
    describe ("Testing " ++ packageName) $ do
      it "The lines beginning with # have been substituted with blank lines" $
        and <$> onAllFiles cabalPath hashesAreProcessed `shouldReturn` True
      it "The package is be parseable with haskell-src-exts" $
        and <$> onAllFiles cabalPath isParsedByHaskellSrcExts `shouldReturn` True

-- Wrapper function (in the spirit of bracket) which, given a package to install
-- and something to do with the cabal file path, initialize a temp directory,
-- does the action and cleans up.
withStackInstalledPackage :: String -> (FilePath -> IO ()) -> IO ()
withStackInstalledPackage packageName action =
  withSystemTempDirectory "preprocessorTest" $ \dir ->
    withCurrentDirectory dir $ do
      execCmd ("\nUnpacking " ++ packageName)
              ("stack unpack " ++ packageName)
      setCurrentDirectory =<< uniqueOutputOf "ls"
      execCmd ("Initializing " ++ packageName)
              ("stack init --solver --ignore-subdirs --silent")
      execCmd ("Building " ++ packageName)
              ("stack build --silent")
      action =<< uniqueOutputOf "ls *.cabal"

-- This function belongs to a new version on System.Directory, which is not yet
-- on hackage. When it's released, I should import it from that package.
withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

-- Executes an action on every file listed as exported library file by the cabal
-- file.
onAllFiles :: CabalFilePath -> (FilePath -> IO a) -> IO [a]
onAllFiles cabalFilePath action = do
  sourceFiles <- getLibExposedModulesPath cabalFilePath
  mapM action sourceFiles

-- Convenience function to retrieve the result of a shell command, assuming it
-- fits in a single line (otherwise it takes the first line).
uniqueOutputOf :: String -> IO String
uniqueOutputOf cmd = do
  (_, stdout, _) <- readCreateProcessWithExitCode (shell cmd) ""
  return . head . lines $ stdout

-- Convenience function to execute the command in a bash-like fashion. The first
-- argument is a message to be printed, the second one the actual command like
-- it would be typed in shell.
execCmd :: String -> String -> IO ()
execCmd message cmd = do
  putStrLn message
  void $ readCreateProcessWithExitCode (shell cmd) ""

--------------------------------------------------------------------
-- Test: Checking that lines beginning with # have been processed --
--------------------------------------------------------------------

hashesAreProcessed :: FilePath -> IO Bool
hashesAreProcessed fp = do
  originFile <- readFile fp
  parsedFile <- preprocessFile fp
  return . and $ zipWith compatible (lines originFile) (lines parsedFile)
  where
    compatible s1@('#':_) s2 = null s2 || s1 == s2
    compatible _       _     = True

--------------------------------------------------------------------
-- Test: check that the files are parseable with haskell-src-exts --
--------------------------------------------------------------------

isParsedByHaskellSrcExts :: FilePath -> IO Bool
isParsedByHaskellSrcExts f = isParseResultOk
                             . parseFileContentsWithComments parseMode
                             <$> preprocessFile f
  where
    isParseResultOk :: ParseResult a -> Bool
    isParseResultOk (ParseOk _) = True
    isParseResultOk _           = False

{- Note [Passing extensions to the haskell-src-exts parser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The parser contained in haskell-src-exts may fail for the absence of the right
extensions coming from the cabal file. A common trick, used for example by:
https://github.com/ndmitchell/hlint/blob/e1c22030721999d4505eb14b19e6f8560a87507a/src/Util.hs
is to import all possible reasonable extensions by default. This solution is
temporary, until I find a better replacement.
-}

parseMode :: ParseMode
parseMode = defaultParseMode { fixities = Nothing, extensions = defaultExtensions }
  where
    defaultExtensions :: [Extension]
    defaultExtensions = [e | e@EnableExtension{} <- knownExtensions]
                        \\ map EnableExtension badExtensions

badExtensions :: [KnownExtension]
badExtensions =
    [ Arrows                     -- steals proc
    , TransformListComp          -- steals the group keyword
    , XmlSyntax, RegularPatterns -- steals a-b
    , UnboxedTuples              -- breaks (#) lens operator
    , QuasiQuotes                -- breaks [x| ...], making whitespace free list comps break
    , DoRec, RecursiveDo         -- breaks rec
    ]

module Test.Utils where

import Control.Exception (bracket)
import System.Directory

-- Here are some utils functions not exported by the version of `directory` used
-- in stackage. This should be deleted once directory reaches a new version.

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

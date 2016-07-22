{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Preprocessor.Types ( Config(..) , GhcParseError(..), defaultConfig)
    where

import Data.List (intercalate)
import Data.Typeable
import Control.Exception (Exception)

import Preprocessor.Loc
import Options.Generic

-- | Parsing errors
data GhcParseError = GhcParseError { loc :: Loc
                                   , msg :: String
                                   } deriving (Typeable)

instance Exception GhcParseError

instance Show GhcParseError where
    show e = tagMsg (loc e) $ fixNewlines (msg e)
        where fixNewlines = intercalate "\n\t\t" . lines

-- | Type holding all the options passed from the command line.
data Config = Config {
  -- | Extension to activate
    exts        :: [String]
    -- | Header files to be automatically included before preprocessing
  , headers     :: [FilePath]
    -- | Additional include directories for the C preprocessor
  , includeDirs :: [FilePath]
  } deriving (Generic, Show)

instance ParseRecord Config

-- | Default configuration options.
--
--   __Warning__: These are not Argon's default options.
defaultConfig :: Config
defaultConfig = Config { exts        = []
                       , headers     = []
                       , includeDirs = []
                       }

{-|
Module      : Preprocessor.Internal.Types
Description : Common types
Copyright   : (c) Carlo Nucera, 2016
License     : BSD3
Maintainer  : meditans@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Preprocessor.Internal.Types
  (
  -- * Options for the cpp preprocessor
    CppOptions (..)
  , emptyCppOptions
  -- * Aliases for the type signatures
  , ProjectDir
  , CabalFilePath
  ) where

--------------------------------------------------------------------------------
-- Options for the preprocessor
--------------------------------------------------------------------------------

-- | `CppOptions` represent the options which are passed, through the ghc api,
-- to the cpp preprocessing program. For reference,
-- <https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html here> is the
-- part of the gcc manual corresponding to the preprocessing options.
data CppOptions = CppOptions
                { cppDefine  :: [String]
                -- ^ CPP #define macros. Corresponds to a @-D@ option for the
                -- cpp program.
                , cppInclude :: [FilePath]
                -- ^ CPP Includes directory. Corresponds to a @-I@ option for
                -- the cpp program.
                , cppFile    :: [FilePath]
                -- ^ CPP pre-include file. Corresponds to a @-include@ option
                -- for the cpp program.
                } deriving (Show)

-- |
-- >>> emptyCppOptions
-- CppOptions {cppDefine = [], cppInclude = [], cppFile = []}
emptyCppOptions :: CppOptions
emptyCppOptions = CppOptions [] [] []

--------------------------------------------------------------------------------
-- Convenient aliases for the type signatures
--------------------------------------------------------------------------------

-- | ProjectDir is the directory which contains the .cabal file for the project.
type ProjectDir    = FilePath
-- | The path of the main @.cabal@ file of the project.
type CabalFilePath = FilePath

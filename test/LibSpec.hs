module LibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Lib

-- `main` is here so that this module can be run from GHCi on its own.

-- Run automatically with:
-- ghcid --command="stack ghci --test preprocessor:test:preprocessor-test" --test=Main.main

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tautologies" $ do
    it "True is True" $ do
      True `shouldBe` False
    it "identity works" $ property $
      \x -> id (x::Bool) == x

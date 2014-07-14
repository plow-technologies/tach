module Tach.Transformable.TypesSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = do
    hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` True

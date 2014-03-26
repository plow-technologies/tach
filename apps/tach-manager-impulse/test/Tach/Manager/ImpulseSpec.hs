module Tach.Manager.ImpulseSpec (main,spec) where


import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction in typeSpec" $ do
    it "should have a definition" $ do
      True `shouldBe` False


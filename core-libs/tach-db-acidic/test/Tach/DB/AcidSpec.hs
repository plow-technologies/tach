module Tach.DB.AcidSpec (main, spec) where

import Test.Hspec
import Tach.DB.Acid.Raw

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "raw acid state functions" $ do
    it "Should open an acid-state of raw pids and then" $ do
      True `shouldBe` "False"

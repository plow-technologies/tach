{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards #-}
module Tach.Manager.Impulse.AppManagerSpec (main,spec) where
import Tach.Manager.Impulse.AppManager
import CorePrelude
import Filesystem
import Data.Default
import Keter.Types
import Keter.Main
import Data.Yaml
import Control.Monad 
import           Filesystem                (createTree, isFile, rename)
import           Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>))
import           Control.Concurrent.Chan
import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Control.Monad.Trans.State as S
import qualified Data.HashMap.Strict       as HMap
import qualified Data.Map as Map
import qualified System.Random             as R


import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction in typeSpec" $ do
    it "should have a definition" $ do
      True `shouldBe` False




testFilePath = "."</>"testCFG"<.>"yaml"

wrtieFilePath = "."</>"test"<.>"txt"

testPlugin :: FilePath -> IO Plugin
testPlugin fp = return $ Plugin { pluginGetEnv = (\a o  -> do 
                                        print "teest" 
                                        writeFile wrtieFilePath "Here is a test bytestring"
                                        return []
                                     )}

testPluginList = [] -- [\x -> (testPlugin x)]






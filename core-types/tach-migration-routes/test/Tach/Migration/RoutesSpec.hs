module Tach.Migration.RoutesSpec (main, spec) where

import Test.Hspec
import Yesod.Test
import Yesod
import Tach.Migration.Routes
import Yesod.Default.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  yesodSpecWithSiteGenerator getSiteAction test

test :: YesodSpec Application
test = ydescribe "Main api" $ do
  yit "Should return 4" $ do
    post ReceiveTimeSeriesR
    bodyEquals "4"

getSiteAction :: IO Application
getSiteAction = undefined -- toWaiApp HelloWorld



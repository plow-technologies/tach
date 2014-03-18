module Tach.Migration.RoutesSpec (main, spec) where

import Test.Hspec
import Yesod.Test
import Yesod
import Tach.Migration.Routes
import Yesod.Default.Config
import Tach.Impulse.Types.TimeValue

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  yesodSpecWithSiteGenerator (return MigrationRoutes) test

-- test :: YesodSpec Application
test = ydescribe "Main api" $ do
  yit "Should return 4" $ do
    post ReceiveTimeSeriesR
    bodyEquals "4"



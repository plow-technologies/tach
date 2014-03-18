module Tach.Migration.RoutesSpec (main, spec) where

import Test.Hspec
import Data.ByteString.Lazy.Char8
import Yesod.Test
import Yesod
import Tach.Migration.Routes
import Yesod.Default.Config
import Tach.Impulse.Types.TimeValue
import Data.Aeson

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  yesodSpecWithSiteGenerator (return MigrationRoutes) test

-- test :: YesodSpec Application
test = ydescribe "Main api" $ do
  yit "Should return 4" $ do
    postBody ReceiveTimeSeriesR body
    bodyEquals $ unpack body
    where body :: ByteString
          body = encode [(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300),(TVSimple 100 10.6 300)]



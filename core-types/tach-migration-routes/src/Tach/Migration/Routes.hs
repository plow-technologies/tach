{-# LANGUAGE QuasiQuotes,TemplateHaskell,TypeFamilies #-}
module Tach.Migration.Routes where

import Tach.Migration.Routes.Internal
import Yesod


import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/migration/receive/time-series-data ReceiveTimeSeriesR POST
|]

instance Yesod HelloWorld


getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

postReceiveTimeSeriesR :: Handler Value
postReceiveTimeSeriesR = do
  return $ toJSON $ four
  where four :: Int
        four = 4
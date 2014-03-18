{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Tach.Migration.Routes where

import Tach.Migration.Routes.Internal
import Yesod
import Yesod.Core
import Yesod.Core.Types
import Data.Aeson
import Tach.Impulse.Types.TimeValue

data MigrationRoutes = MigrationRoutes

mkYesod "MigrationRoutes" [parseRoutes|
/ HomeR GET
/migration/receive/time-series-data ReceiveTimeSeriesR POST
|]

instance Yesod MigrationRoutes


instance FromJSON TVSimple where
instance ToJSON TVSimple where

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Simple API|]

postReceiveTimeSeriesR :: Handler Value
postReceiveTimeSeriesR = do
  tsInfo <- parseJsonBody :: Handler (Result [TVSimple])
  return $ toJSON $ four
  where four :: Int
        four = 4
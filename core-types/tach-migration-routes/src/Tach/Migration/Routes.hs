{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings #-}
module Tach.Migration.Routes where

import Tach.Migration.Routes.Internal

import Control.Applicative
import Data.Aeson
import Data.Either
import Data.Text
import Data.ByteString.Lazy
import Tach.Impulse.Types.TimeValue
import Network.HTTP.Types

import Yesod
import Yesod.Core
import Yesod.Core.Types

data MigrationRoutes = MigrationRoutes

mkYesod "MigrationRoutes" [parseRoutes|
/ HomeR GET
/migration/receive/time-series-data ReceiveTimeSeriesR POST
|]

instance Yesod MigrationRoutes


instance FromJSON TVSimple where
  parseJSON (Object s) = TVSimple <$>
              s .: "time"      <*>
              s .: "val"     <*>
              s .: "id"
instance ToJSON TVSimple where
  toJSON TVSimple{..} = object [ "time" .= tvSimpleTime
                                ,"val"  .= tvSimpleTime
                                ,"id"   .= tvSimpleId
                               ]
  
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Simple API|]

postReceiveTimeSeriesR :: Handler Value
postReceiveTimeSeriesR = do
  tsInfo <- parseJsonBody :: Handler (Result [TVSimple])
  resultEither (return . toJSON) (\x -> sendResponseStatus (Status 400 (toStrict $ encode x)) (x)) tsInfo
  

resultEither :: ToJSON a => (a -> c) -> (String -> c) -> Result a -> c
resultEither _ failure (Error s) = failure s
resultEither success _ (Success a) = success a
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Tach.Query.Foundation where

import Persist.Mongo.Settings
import Yesod

data QueryYesod = QueryYesod {
  mongoConf :: MongoDBConf
}

mkYesodData "QueryYesod" $(parseRoutesFile "tach-query-routes")
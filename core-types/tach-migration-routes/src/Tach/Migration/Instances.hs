{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings #-}
module Tach.Migration.Instances where

import Control.Applicative
import Data.Aeson
import Tach.Impulse.Types.TimeValue

instance FromJSON TVNoKey where
  parseJSON (Object s) = TVNoKey <$>
              s .: "time"      <*>
              s .: "val" 
instance ToJSON TVNoKey where
  toJSON TVNoKey{..} = object [  "time" .= tvNkSimpleTime
                                ,"val"  .= tvNkSimpleValue
                               ]
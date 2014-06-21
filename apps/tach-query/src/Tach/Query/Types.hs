{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}
module Tach.Query.Types where

import Data.Time
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Data.Text

data ParameterQuery = ParameterQuery {
      phistoryStep :: Double
     ,phistoryDelta :: Double
     ,phistoryStart :: UTCTime
     ,phistoryEnd   :: UTCTime
     ,phistoryPID :: Text
} deriving (Eq, Show)

instance FromJSON ParameterQuery where
  parseJSON (Object p) = ParameterQuery <$>
                          p .: "step"  <*>
                          p .: "delta" <*>
                          p .: "start" <*>
                          p .: "end"   <*>
                          p .: "pid"
  parseJSON _ = fail "Rule: Expecting Object {delta:<val>,pid:<val>,step:<val>,start:<val>,end:<val>}"
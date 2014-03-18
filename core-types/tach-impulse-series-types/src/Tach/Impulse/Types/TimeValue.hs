{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Tach.Impulse.Types.TimeValue where 


import Data.Thyme -- A faster time library 

import GHC.Generics
import Data.Typeable
import Tach.Impulse.Types.Impulse 

type TVTypeOfTime = NominalDiffTime
type TVPeriod  = ImpulsePeriod  TVTypeOfTime TVTypeOfTime 

data TVSimple = TVSimple{
  tvSimpleTime  :: UTCTime
 ,tvSimpleValue :: Double
 ,tvSimpleId    :: Int 
} deriving (Read, Show, Eq, Generic, Typeable)
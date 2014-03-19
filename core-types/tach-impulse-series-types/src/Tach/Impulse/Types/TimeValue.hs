{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Tach.Impulse.Types.TimeValue where 

import GHC.Generics
import Data.Typeable
import Tach.Impulse.Types.Impulse 
import Data.Typeable (Typeable)
import Data.Vector
type TVTypeOfTime = Integer
type TVParameterType = Vector Double
type TVPeriod  = ImpulsePeriod TVParameterType TVTypeOfTime 

data TVSimple = TVSimple{
  tvSimpleTime  :: Integer
 ,tvSimpleId    :: Int 
 ,tvSimpleValue :: Double
} deriving (Show,Generic,Typeable,Ord,Eq)





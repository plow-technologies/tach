{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Tach.Impulse.Types.TimeValue where 



import GHC.Generics
import Data.Typeable
import Tach.Impulse.Types.Impulse 
import Data.Typeable (Typeable)

type TVTypeOfTime = Integer
type TVPeriod  = ImpulsePeriod  TVTypeOfTime TVTypeOfTime 

data TVSimple = TVSimple{
  tvSimpleTime  :: Integer
 ,tvSimpleValue :: Double
 ,tvSimpleId    :: Int 
} deriving (Show,Generic,Typeable)
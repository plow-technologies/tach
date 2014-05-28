{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Tach.Impulse.Types.TimeValue where 

import GHC.Generics
import Data.Typeable
import Tach.Impulse.Types.Impulse 
import Data.Typeable (Typeable)
import Data.Vector
import Tach.Migration.Types
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK

type TVKey = ImpulseKey (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
type TVTypeOfTime = Int
type TVParameterType = Vector Double
type TVPeriod  = ImpulsePeriod TVParameterType TVTypeOfTime 

data TVSimple = TVSimple{
  tvSimpleTime  :: Int
 ,tvSimpleId    :: Int 
 ,tvSimpleValue :: Double
} deriving (Show,Generic,Typeable,Ord,Eq)


data TVNoKey = TVNoKey {
  tvNkSimpleTime  :: Int
 ,tvNkSimpleValue :: Double
} deriving (Show,Generic,Typeable,Ord,Eq)

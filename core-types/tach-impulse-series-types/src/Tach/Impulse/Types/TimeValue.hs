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
<<<<<<< HEAD
 ,tvSimpleValue :: Double
} deriving (Show,Generic,Typeable,Ord,Eq)





=======
} deriving (Show,Generic,Typeable)
>>>>>>> 1b15174117a0c99b3fc01a24aa989f7e8db97b28

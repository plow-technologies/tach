{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Tach.Impulse.Types.TimeValueSeries where 


import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValue
import Data.Vector
import Data.Set

{- | Time Value Series are based on the impulse series, and designed to be in the time domain.  
     They may be based on a fixed period or a parameterized one.  
|-}


-- | The kind of time used for the time value series
type TVSTypeOfTime = TVTypeOfTime
type TVSPeriod = TVPeriod
type TVSKey = TVKey
type TVSStart = ImpulseStart TVSTypeOfTime
type TVSEnd = ImpulseEnd TVSTypeOfTime


-- | TimeValueSeries are the generic representations of a series of values in the time domain.
-- What is meant by 'time domain' and 'value' are left to the specific implementation
type TimeValueSeries rep = ImpulseSeries TVSKey TVSPeriod TVSStart TVSEnd (ImpulseRep rep) 


-- | TimeValue Standard Type is Double and this is the  periodic version
type TVSDoublePeriodicType = TimeValueSeries (Vector Double)


-- | TimeValue Standard Type is TVSimple and this is the  impulse version
type TVSimpleImpulseType = TimeValueSeries (Set TVNoKey)





-- /Tach/Impulse/Types/

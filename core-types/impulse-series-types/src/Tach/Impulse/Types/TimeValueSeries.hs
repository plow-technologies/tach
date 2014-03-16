{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Tach.Impulse.Types.TimeValueSeries where 

import Data.Thyme -- A faster time library 
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValue
import Data.Vector

{- | Time Value Series are based on the impulse series, and designed to be in the time domain.  
     They may be based on a fixed period or a parameterized one.  
|-}


-- | The kind of time used for the time value series
type TVSTypeOfTime = TVTypeOfTime
type TVSPeriod = TVPeriod

type TVSStart = ImpulseStart TVSTypeOfTime
type TVSEnd = ImpulseEnd TVSTypeOfTime


-- | TimeValueSeries are the generic representations of a series of values in the time domain.
-- What is meant by 'time domain' and 'value' are left to the specific implementation
type TimeValueSeries rep = ImpulseSeries TVSPeriod TVSStart TVSEnd (ImpulseRep rep) 


-- | TimeValue Standard Type is Double
type TVSDoubleType = TimeValueSeries (Vector Double)




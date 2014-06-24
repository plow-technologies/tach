{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleInstances, NoMonomorphismRestriction, NoImplicitPrelude #-}

module Tach.Acid.Impulse.Lens where

import BasicPrelude 

-- Lens Specific
import Control.Lens
-- import Control.Lens.TH

-- Impulse Specific 
-- import Tach.Acid.Impulse.State 

import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.Impulse
-- import Tach.Impulse.Types.TimeValueSeries (TVSStart,TVSEnd)
import Tach.Migration.Acidic.Types
-- import Tach.Migration.Acidic.Instances 


{-| These lenses are here for use with Acid to make it easier to update the state of a particular entity |-} 



makeClassy_ ''TVSimpleImpulseTypeStore 
makeClassy_ ''TVNoKey
makeClassy_ ''ImpulseSeries 
makeClassy_ ''ImpulseKey
makeClassy_ ''ImpulseStart 
makeClassy_ ''ImpulseEnd 
makeClassy_ ''ImpulseRep



_TVSimpleImpulseRep :: Lens' TVSimpleImpulseTypeStore (Set TVNoKey)
_TVSimpleImpulseRep = _unTimeValueStore . _impulseSeriesRep . _unRep


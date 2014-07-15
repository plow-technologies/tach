{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Tach.DB.Acid.Raw.Lens where

import           BasicPrelude
import           Control.Lens
import           Tach.DB.Acid.Raw.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue



{-| These lenses are here for use with Acid to make it easier to update the state of a particular entity |-}
makeClassy_ ''TVSimpleRawStore
makeClassy_ ''TVNoKey
makeClassy_ ''ImpulseSeries
makeClassy_ ''ImpulseKey
makeClassy_ ''ImpulseStart
makeClassy_ ''ImpulseEnd
makeClassy_ ''ImpulseRep
makeClassy_ ''RawStart
makeClassy_ ''RawEnd
makeClassy_ ''RawSeries



_TVSimpleRawRep :: Lens' TVSimpleRawStore (Set TVNoKey)
_TVSimpleRawRep = _unTVSimpleRawStore . _rawSeriesRep
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Tach.DB.Acid.Transform.Lens where

import           BasicPrelude
import           Control.Lens
import           Tach.DB.Acid.Transform.Types
import           Tach.DB.Types
import           Tach.DB.Types.Transform.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue



{-| These lenses are here for use with Acid to make it easier to update the state of a particular entity |-}
makeClassy_ ''TVSimpleTransformStore
makeClassy_ ''TVNoKey
makeClassy_ ''ImpulseSeries
makeClassy_ ''ImpulseKey
makeClassy_ ''ImpulseStart
makeClassy_ ''ImpulseEnd
makeClassy_ ''ImpulseRep
makeClassy_ ''TransformStart
makeClassy_ ''TransformEnd
makeClassy_ ''TransformSeries



_tvSimpleTransformRep :: Lens' TVSimpleTransformStore (Set TransformedInformation)
_tvSimpleTransformRep = _unTVSimpleTransformStore . _transformSeriesRep

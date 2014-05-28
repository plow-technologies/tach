{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tach.Migration.Acidic.Instances where

import Tach.Migration.Acidic.Types 
import Tach.Impulse.Types.TimeValue 
--import Tach.Impulse.Types.TimeValueSeries 
import Tach.Impulse.Types.Impulse
-- import Data.Typeable (Typeable)
-- import Data.Sequence
-- import Data.IntMap
-- import Data.Thyme
-- import Data.Vector
-- import GHC.Generics
import Data.SafeCopy        ( base, deriveSafeCopy )

import qualified DirectedKeys.Types as DK
import Tach.Migration.Types


-- Safe Copy Derivations for types we depend on
$(deriveSafeCopy 0 'base ''DK.DirectedKeyRaw)


$(deriveSafeCopy 0 'base ''KeyTime)
$(deriveSafeCopy 0 'base ''KeyPid)
$(deriveSafeCopy 0 'base ''KeySource)
$(deriveSafeCopy 0 'base ''KeyDestination)

$(deriveSafeCopy 0 'base ''ImpulseKey)
$(deriveSafeCopy 0 'base ''ImpulseStart)
$(deriveSafeCopy 0 'base ''ImpulseEnd)
$(deriveSafeCopy 0 'base ''ImpulseRep)
$(deriveSafeCopy 0 'base ''TVSimple) 
$(deriveSafeCopy 0 'base ''ImpulsePeriod)
$(deriveSafeCopy 0 'base ''ImpulseSeries)


-- Our Safecopy instance
$(deriveSafeCopy 0 'base ''TVSimpleImpulseTypeStore) 
$(deriveSafeCopy 0 'base ''IntKey)
$(deriveSafeCopy 0 'base ''TVNoKey)

$(deriveSafeCopy 0 'base ''ImpulseMap)




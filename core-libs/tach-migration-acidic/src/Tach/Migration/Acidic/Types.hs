{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Tach.Migration.Acidic.Types where

import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.TimeValueSeries 
import Tach.Impulse.Types.Impulse
import Data.Typeable (Typeable)
import Data.Sequence
import Data.IntMap
-- import Data.Thyme
import Data.Vector
import GHC.Generics
import Data.SafeCopy        ( base, deriveSafeCopy )
-- | The Acid State instance of anything is that thing suffixed with Store... 'TimeValue' -> 'TimeValueStore'
-- This prevents confusion later 

-- | This is a simple impulse valued sequence.  No Compression, No structural change.
-- Note, the terrible type signature occurs because safecopy hates type synonyms!
newtype TVSimpleImpulseTypeStore = TVSimpleImpulseTypeStore { unTimeValueStore :: (ImpulseSeries (ImpulsePeriod (Vector Double) Integer ) (ImpulseStart Integer) (ImpulseEnd Integer) (ImpulseRep (Seq TVSimple))) } deriving (Typeable,Generic)



{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Migration.Acidic.State where

-- import Tach.Migration.Acidic.State.Internal
import Tach.Migration.Acidic.Types
-- import Tach.Migration.Acidic.Instances
import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.TimeValueSeries
import Tach.Impulse.Types.Impulse
import qualified Data.Sequence as S
import qualified Data.IntMap as M
import qualified Data.Vector as V



-- Intial State defnitions

-- | This is a simple impulse valued sequence.  No Compression, No structural change.
-- Note, the terrible type signature occurs because safecopy hates type synonyms!
-- newtype TVSimpleImpulseTypeStore = TVSimpleImpulseTypeStore { unTimeValueStore :: (ImpulseSeries (ImpulsePeriod (Vector Double) Integer ) (ImpulseStart Integer) (ImpulseEnd Integer) (ImpulseRep (Seq TVSimple))) } deriving (Typeable,Generic)


initialImpulseStart :: TVSStart -- ImpulseStart Integer
initialImpulseStart       = ImpulseStart 0

initialImpulseEnd :: TVSEnd -- ImpulseEnd Integer
initialImpulseEnd       = ImpulseEnd 0

initialImpulseRep :: ImpulseRep (S.Seq TVSimple)
initialImpulseRep       = ImpulseRep (S.empty)

-- | IntiialTVSimple not needed cause the rep is empty
-- initialTVSimple       = 
-- initialTVSimple       = undefined 


initialImpulsePeriod :: TVPeriod -- ImpulsePeriod (V.Vector Double) (Integer)
initialImpulsePeriod       = IPeriodParameterized (V.empty)
initialImpulseSeries ::     TVSimpleImpulseType
initialImpulseSeries       = ImpulseSeries initialImpulsePeriod initialImpulseStart initialImpulseEnd initialImpulseRep 

initialTVSimpleImpulseTypeStore :: TVSimpleImpulseTypeStore 
initialTVSimpleImpulseTypeStore       = TVSimpleImpulseTypeStore initialImpulseSeries 

initialIntKey :: IntKey
initialIntKey = IntKey ""

initialImpulseMap :: ImpulseMap
initialImpulseMap  = ImpulseMap $ M.fromList [(0,initialIntKey)]



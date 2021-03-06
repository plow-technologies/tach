{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Acid.Impulse.State where

-- import Tach.Migration.Acidic.State.Internal
import Tach.Migration.Acidic.Types
-- import Tach.Migration.Acidic.Instances
import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.TimeValueSeries
import Tach.Impulse.Types.Impulse
import qualified Data.Set as S
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK
import Tach.Migration.Types



-- Intial State defnitions




initialImpulseKey :: TVKey
initialImpulseKey = ImpulseKey (DK.DKeyRaw (KeyPid 0) (KeySource "") (KeyDestination "") (KeyTime 0))

initialImpulseStart :: TVSStart -- ImpulseStart Integer
initialImpulseStart       = ImpulseStart 0

initialImpulseEnd :: TVSEnd -- ImpulseEnd Integer
initialImpulseEnd       = ImpulseEnd 0

initialImpulseRep :: ImpulseRep (S.Set TVNoKey)
initialImpulseRep       = ImpulseRep (S.empty)

-- | IntiialTVSimple not needed cause the rep is empty
-- initialTVSimple       = 
-- initialTVSimple       = undefined 


initialImpulsePeriod :: TVPeriod -- ImpulsePeriod (V.Vector Double) (Integer)
initialImpulsePeriod       = IPeriodParameterized (V.empty)


initialImpulseSeries ::     TVSimpleImpulseType
initialImpulseSeries       = ImpulseSeries initialImpulseKey initialImpulsePeriod initialImpulseStart initialImpulseEnd initialImpulseRep 

initialTVSimpleImpulseTypeStore :: TVSimpleImpulseTypeStore 
initialTVSimpleImpulseTypeStore       = TVSimpleImpulseTypeStore initialImpulseSeries 

initialIntKey :: IntKey
initialIntKey = IntKey ""

initialImpulseMap :: ImpulseMap
initialImpulseMap  = ImpulseMap $ M.fromList [(0,initialIntKey)]



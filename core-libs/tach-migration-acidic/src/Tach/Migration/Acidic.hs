{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Tach.Migration.Acidic where

-- import Tach.Migration.Acidic.Internal



import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.TimeValueSeries 
import Tach.Impulse.Types.Impulse
import Data.Typeable (Typeable)
import Data.Sequence
import Data.Graph
-- import Data.Thyme
import GHC.Generics
import Data.SafeCopy        ( base, deriveSafeCopy )
-- | The Acid State instance of anything is that thing suffixed with Store... 'TimeValue' -> 'TimeValueStore'
-- This prevents confusion later 

newtype TVSimpleStore = TVSimpleStore { unTimeValueStore :: TVSimple } deriving (Typeable,Generic)



instance Eq TVSimpleStore where 
  (==) (TVSimpleStore (TVSimple tm v i)) (TVSimpleStore (TVSimple tm' v' i')) = and [tm == tm', v == v', i == i']
-- | Ord by pid, time , Value
instance Ord TVSimpleStore where 
  compare = compareSimpleStore


compareSimpleStore (TVSimpleStore (TVSimple tm v i)) (TVSimpleStore (TVSimple tm' v' i')) 
    | tm < tm' = LT 
    | tm > tm' = GT
    | i  < i'  = LT 
    | i  > i'  = GT 
    | v  <= v' = LT 
    | v  >  v' = GT


type StorableSeries = TimeValueSeries TVSimpleStore

newtype TimeValueSeriesStore = TimeValueSeriesStore { unTimeValueSeriesStore :: (StorableSeries)} 
    deriving (Typeable,Generic)

$(deriveSafeCopy 0 'base ''TVSimple) 
$(deriveSafeCopy 0 'base ''TVSimpleStore) 
$(deriveSafeCopy 0 'base ''ImpulseSeries)
$(deriveSafeCopy 0 'base ''ImpulseRep)
$(deriveSafeCopy 0 'base ''ImpulseStart)
$(deriveSafeCopy 0 'base ''ImpulseEnd)
$(deriveSafeCopy 0 'base ''ImpulsePeriod)
$(deriveSafeCopy 0 'base ''TimeValueSeriesStore)




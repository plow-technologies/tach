{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Tach.Acid.Impulse.Cruds.Delete where

import CorePrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Set
import Filesystem.Path

-- ACID Specific
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )

import Data.SafeCopy        ( base, deriveSafeCopy )
-- import Data.Typeable (Typeable)

-- Lens Specific 
import Tach.Acid.Impulse.Lens
import Control.Lens (over,views, (^.), set, view)
import Filesystem.Path
import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValueSeries (TVSStart,TVSEnd)
import Tach.Migration.Acidic.Types
import Tach.Acid.Impulse.Cruds.Types

deleteTVSimpleImpulse :: TVKey -> (TVNoKey) -> Update TVSimpleImpulseTypeStore (Either ErrorValue SuccessValue)
deleteTVSimpleImpulse tk d = do
  st@(TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) <- get
  case st of
    _
      | k == tk -> put st'' >> (return . Right $ SuccessValue . LB.toStrict . encode . object $ ["setSize" .= (sz)])
      | otherwise -> return . Left $ ErrorValue ErrorIncorrectKey
     where
      st' = (over _unTimeValueStore (removeTVKey) st)
      newSet = (view (_unTimeValueStore . _impulseSeriesRep . _unRep) st')
      st'' = (over _unTimeValueStore (updateLower . updateHigher) st')
      sz = views _TVSimpleImpulseRep size st'
      remove = (\set -> d `delete` set)
      removeTVKey = (over (_impulseSeriesRep . _unRep) remove)
      updateHigher = (set (_impulseSeriesEnd . _unEnd) (tvNkSimpleTime $ findMax newSet) )
      updateLower = (set (_impulseSeriesStart . _unStart) (tvNkSimpleTime $ findMin newSet) )

deleteManyTVSimpleImpulse :: TVKey -> (Set TVNoKey) -> Update TVSimpleImpulseTypeStore (Either ErrorValue SuccessValue)
deleteManyTVSimpleImpulse tk ds = do
  st@(TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) <- get
  case st of
    _
      | k == tk -> put st'' >> (return . Right $ SuccessValue . LB.toStrict . encode . object $ ["setSize" .= (sz)])
      | otherwise -> return . Left $ ErrorValue ErrorIncorrectKey
      where
        st' = (over _unTimeValueStore (removeTVKeys) st)
        newSet = (view (_unTimeValueStore . _impulseSeriesRep . _unRep) st')
        st'' = (over _unTimeValueStore (updateLower . updateHigher) st')
        sz = views _TVSimpleImpulseRep size st'
        setMinus = (\set -> set `difference` ds)
        removeTVKeys = (over (_impulseSeriesRep . _unRep) setMinus)
        updateHigher = (set (_impulseSeriesEnd . _unEnd) (tvNkSimpleTime $ findMax newSet) )
        updateLower = (set (_impulseSeriesStart . _unStart) (tvNkSimpleTime $ findMin newSet) )

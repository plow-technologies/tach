{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Acid.Impulse.Cruds.Create where
-- Generic Haskell Stuff 

import Control.Applicative
import qualified Data.Traversable as T
import Filesystem.Path
import CorePrelude
import Data.Aeson
-- Containers 
import Data.Set
--  import Data.IntMap hiding (union, insert)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy as LB
-- ACID Specific
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )
-- import Data.Acid.Advanced   ( query', update' )
-- import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
-- import Data.Typeable (Typeable)

-- Lens Specific 
import Tach.Acid.Impulse.Lens
import Control.Lens (over,views, (^.) )

-- Impulse Specific 
-- import Tach.Acid.Impulse.State 

import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValueSeries (TVSStart,TVSEnd)
import Tach.Migration.Acidic.Types
import Tach.Acid.Impulse.Cruds.Types

-- import Tach.Migration.Acidic.Instances 


-- |Create an ImpulseTypeStore and return the filepath it was created

-- createTVSimpleImpulseTypeStore :: (ImpulseKey Integer) (ImpulsePeriod (Vector Double) (Integer)) (ImpulseStart Integer) (ImpulseEnd Integer) (ImpulseRep (Seq TVNoKey))



-- |ACID Functions



-- | Insert one element into an ImpulseTypeStore , return the number now in the store 
-- | Insert also updates the bounds of the Data Set

insertTVSimpleImpulse :: TVKey -> (TVNoKey) -> Update TVSimpleImpulseTypeStore (Either ErrorValue SuccessValue)
insertTVSimpleImpulse tk d = do
  st@(TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) <- get 
  case st of 
    _ 
      | k == tk -> put st' >> (return . Right $ SuccessValue . LB.toStrict . encode . object $ ["setSize" .= (sz)])
      | otherwise -> return . Left $ ErrorValue ErrorIncorrectKey 
     where
      st' =   (over _unTimeValueStore (insertIfNewer.insertIfOlder.insertTimeValue) st )
      sz  = views _TVSimpleImpulseRep size st'
      appendData = (\s -> (d `insert` s) )
      insertTimeValue = (over (_impulseSeriesRep . _unRep ) appendData)
      insertIfOlder = (over (_impulseSeriesStart . _unStart) useOlder )      
      insertIfNewer = (over (_impulseSeriesEnd . _unEnd) useNewer )      
      useOlder i = case (d ^. _tvNkSimpleTime) of 
                  j 
                       | i <= j -> i -- i still lower bound
                       | otherwise -> j -- replace lower bound
          
      useNewer i = case (d ^. _tvNkSimpleTime) of 
                  j 
                       | i >= j -> i -- i still lower bound
                       | otherwise -> j -- replace lower bound
          
                                                            
      

-- | Like above but batch insert
insertManyTVSimpleImpulse :: TVKey ->  (Set TVNoKey) -> Update TVSimpleImpulseTypeStore (Either ErrorValue SuccessValue)
insertManyTVSimpleImpulse tk ds = do 
  st@(TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) <- get 
  let mn = findMin ds
      mx = findMax ds
  case st of 
    _ 
      | k == tk -> put st' >> (return . Right $ SuccessValue . LB.toStrict .encode . object $ ["setSize" .= (sz)])
      | otherwise -> return . Left $ ErrorValue ErrorIncorrectKey
     where
      st' =  over _unTimeValueStore (insertIfNewer . insertIfOlder . insertTimeValues) st
      sz  = views _TVSimpleImpulseRep size st'
      appendData = (\s -> ds `union` s)
      insertTimeValues = (over (_impulseSeriesRep . _unRep ) appendData)
      insertIfOlder = (over (_impulseSeriesStart . _unStart) useOlder )      
      insertIfNewer = (over (_impulseSeriesEnd . _unEnd) useNewer )      
      useOlder i = case (mn ^. _tvNkSimpleTime) of 
                  j 
                       | i <= j -> i -- i still lower bound
                       | otherwise -> j -- replace lower bound
          
      useNewer i = case (mx ^. _tvNkSimpleTime) of 
                  j 
                       | i >= j -> i -- i still lower bound
                       | otherwise -> j -- replace lower bound
      


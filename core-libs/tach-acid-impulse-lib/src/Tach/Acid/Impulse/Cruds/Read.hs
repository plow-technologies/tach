{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
 GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Acid.Impulse.Cruds.Read where
-- Generic Haskell Stuff 

import Control.Applicative
import qualified Data.Traversable as T
import Filesystem.Path
import CorePrelude
import Data.Aeson

-- Containers 
import Data.Set
-- import Data.IntMap
import Data.Vector (Vector)
-- import Data.ByteString

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
import Control.Lens (over,views,view)

import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValueSeries (TVSStart,TVSEnd)
import Tach.Migration.Acidic.Types
import qualified Data.ByteString.Lazy as LB
import Tach.Acid.Impulse.Cruds.Types

elookupLE :: Ord a => a -> Set a -> Either ErrorValue a
elookupLE a s = case lookupLE a s of 
                  Nothing -> Left $ ErrorValue ErrorNotFound
                  Just l -> Right l

-- | get the value closest to the time given in a set

getTVSimpleImpulse :: TVKey -> Integer ->  Query TVSimpleImpulseTypeStore (Either ErrorValue TVNoKey)
getTVSimpleImpulse tk tm = queryFcn <$> ask
 where 
   fakeTvNoKey = TVNoKey tm 0.0 -- fake data entry used to query the db
   queryFcn st
       |(isKey st) && (timeIsIn st) = views _TVSimpleImpulseRep (\s -> elookupLE fakeTvNoKey s) st
       |otherwise   = Left $ ErrorValue ErrorOutOfBounds
   timeIsIn st = let strtV = view (_unTimeValueStore._impulseSeriesStart . _unStart) st
                     endV  = view (_unTimeValueStore._impulseSeriesEnd . _unEnd ) st
                 in (strtV <= tm) && (endV >= tm)
   isKey (TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) = k == tk 
       

-- | Get the Set of values in between a start and end value at a given sample rate 
getTVSimpleImpulseMany :: TVKey -> TVSStart -> TVSEnd -> Query TVSimpleImpulseTypeStore (Either ErrorValue (Set TVNoKey))
getTVSimpleImpulseMany tk tstart tend 
    | (unStart tstart) <= (unEnd tend) = queryFcn <$> ask
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
    where
      fakeTvNoKeyStart = TVNoKey (unStart tstart) 0.0
      fakeTvNoKeyEnd = TVNoKey (unEnd tend) 0.0
      isKey (TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) = k == tk 
      trimOffLess s = snd $ split fakeTvNoKeyStart s
      trimOffMore s = fst $ split fakeTvNoKeyEnd s
      queryFcn st 
          |(isKey st) = Right $  (views _TVSimpleImpulseRep (trimOffMore.trimOffLess) st)
          |otherwise = (Left $ ErrorValue ErrorIncorrectKey)

getTVSimpleImpulseSize :: TVKey -> Query TVSimpleImpulseTypeStore (Either ErrorValue Int)
getTVSimpleImpulseSize tk = undefined

getTVSimpleImpulseTimeBounds :: TVKey -> Query TVSimpleImpulseTypeStore (Either ErrorValue (ImpulseStart Int, ImpulseEnd Int))
getTVSimpleImpulseTimeBounds tk = undefined --doPro
  --queryFcn <$> ask
  --where isKey (TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k})) = k == tk
  --queryFcn st
  --  | (isKey st) = Right $ (views _TVSimpleImpulseRep size st)
  --  | otherwise = (Left $ ErrorValue ErrorIncorrectKey)
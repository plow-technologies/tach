{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}

module Tach.Transformable.Types.Impulse where

import qualified Data.Foldable                as F
import           Data.Function
import           Data.Maybe
import qualified Data.Sequence                as S
import           Data.Typeable
import           Tach.Class.Bounds
import qualified Tach.Class.Insertable        as I
import           Tach.Class.Queryable
import           Tach.Impulse.Types.TimeValue
import           Tach.Types.Classify


data ImpulseTransformed = ImpulseTransformed {
    impulseRepresentation :: S.Seq TVNoKey
  , impulseStart          :: Int
  , impulseEnd            :: Int
} deriving (Show, Ord, Eq, Typeable)


instance Bound ImpulseTransformed where
  bounds = impulseBounds

instance Queryable ImpulseTransformed TVNoKey where
  query step start end impls = I.toInsertable $ queryImpulse impls step start end


reconstructImpulse :: ImpulseTransformed -> [TVNoKey]
reconstructImpulse = F.toList . impulseRepresentation

queryImpulse :: ImpulseTransformed -> Int -> Int -> Int -> S.Seq TVNoKey
queryImpulse tf step start end = trim . impulseRepresentation $ tf
  where trim = (S.dropWhileL (\x -> (tvNkSimpleTime x) >= start)) . (S.dropWhileR (\x -> (tvNkSimpleTime x) <= end))

transformImpulse :: [TVNoKey] -> ImpulseTransformed
transformImpulse tvnklist = ImpulseTransformed rep start end
  where rep = (S.unstableSortBy (compare `on` tvNkSimpleTime)) . S.fromList $ tvnklist -- Ensure that the list is sorted on time
        start = tvNkSimpleTime . headSeq $ rep
        end = tvNkSimpleTime . lastSeq $ rep


impulseBounds :: ImpulseTransformed -> (Int, Int)
impulseBounds impls = (impulseStart impls, impulseEnd impls)


headSeq :: S.Seq a -> a
headSeq = fromJust . headMaySeq

headMaySeq :: S.Seq a -> Maybe a
headMaySeq aSeq =
  if (len >= 1)
    then
      Just $ S.index aSeq (len - 1)
    else
      Nothing
  where len = S.length aSeq


lastSeq :: S.Seq a -> a
lastSeq = fromJust . lastMaySeq

lastMaySeq :: S.Seq a -> Maybe a
lastMaySeq aSeq =
  if (len >= 1)
    then
      Just $ S.index aSeq (len - 1)
    else
      Nothing
  where len = S.length aSeq

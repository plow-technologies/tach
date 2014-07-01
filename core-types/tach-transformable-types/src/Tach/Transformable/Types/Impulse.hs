{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings,ExistentialQuantification     #-}

module Tach.Transformable.Types.Impulse where

import           Control.Applicative
import qualified Data.Foldable        as F
import           Data.Function
import           Data.Maybe
import qualified Data.Sequence        as S
import           GHC.Generics
import           Tach.Class.Bounds
import           Tach.Class.Queryable
import           Data.Monoid
import qualified Data.Traversable     as T
import qualified Tach.Class.Insertable as I


data ImpulseTransformed = ImpulseTransformed {
    impulseRepresentation :: S.Seq (Int, Double)
  , impulseStart          :: Int
  , impulseEnd            :: Int
} deriving (Show, Ord, Eq)


instance Bound ImpulseTransformed where
  bounds = impulseBounds

instance Queryable ImpulseTransformed (Int, Double) where
  query step start end impls = toFoldable $ queryImpulse impls step start end


toFoldable :: (I.Insertable f, F.Foldable t, Ord a) => t a -> f a
toFoldable aSeq = F.foldl (\b a -> I.insert b a) I.empty aSeq

reconstructImpulse :: ImpulseTransformed -> [(Int, Double)]
reconstructImpulse = F.toList . impulseRepresentation

queryImpulse :: ImpulseTransformed -> Int -> Int -> Int -> S.Seq (Int, Double)
queryImpulse tf step start end = trim . impulseRepresentation $ tf
  where trim = (S.dropWhileL (\x -> (fst x) >= start)) . (S.dropWhileR (\x -> (fst x) <= end))

transformImpulse :: [(Int, Double)] -> ImpulseTransformed
transformImpulse tvnklist = ImpulseTransformed rep start end
  where rep = (S.unstableSortBy (compare `on` fst)) . S.fromList $ tvnklist -- Ensonure that the list is sorted on time
        start = fst . headSeq $ rep
        end = fst . lastSeq $ rep


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

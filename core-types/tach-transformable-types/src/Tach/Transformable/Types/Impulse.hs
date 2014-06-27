{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tach.Transformable.Types.Impulse where

import           Control.Applicative
import qualified Data.Foldable       as F
import           Data.Function
import           Data.Maybe
import qualified Data.Sequence       as S
import           GHC.Generics

data ImpulseTransformed = ImpulseTransformed {
    impulseRepresentation :: S.Seq (Int, Double)
  , impulseStart          :: Int
  , impulseEnd            :: Int
} deriving (Show)


reconstructImpulse :: ImpulseTransformed -> [(Int, Double)]
reconstructImpulse = F.toList . impulseRepresentation

queryImpulse :: ImpulseTransformed -> Int -> Int -> Int -> [(Int, Double)]
queryImpulse tf step start end = F.toList . trim . impulseRepresentation $ tf
  where trim = (S.dropWhileL (\x -> (fst x) >= start)) . (S.dropWhileR (\x -> (fst x) <= end))

transformImpulse :: [(Int, Double)] -> ImpulseTransformed
transformImpulse tvnklist = ImpulseTransformed rep start end
  where rep = (S.unstableSortBy (compare `on` fst)) . S.fromList $ tvnklist -- Ensonure that the list is sorted on time
        start = fst . headSeq $ rep
        end = fst . lastSeq $ rep

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
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}{-# LANGUAGE TypeFamilies #-}

module Tach.Transformable.Types.Impulse where

import qualified Data.Foldable                as F
import           Data.Function
import           Data.Maybe
import qualified Data.Sequence                as S
import           Data.Typeable
import           Numeric.Classes.Indexing
import           Numeric.Tools.Mesh
import           Tach.Class.Bounds
import qualified Tach.Class.Insertable        as I
import           Tach.Class.Queryable
import           Tach.Impulse.Types.TimeValue
import           Tach.Types.Classify
import qualified Data.Vector as V
import Statistics.Function
import Control.Applicative
import Numeric.Tools.Interpolation
import           Data.Wavelets.Construction
import           Data.Wavelets.Reconstruction
import           Data.Wavelets.Scaling
import           Tach.Transformable.Types.Wavelet.Core

xs :: [Int]
xs = [0,15,30,60,75,90,105,143,162,201,206,209,250,260,270,280,295,301,310,328,335,354,378,394,401,419,434,449,479,494,509,524,562,581,620,625,628,669,679,689,699,714,720,729,747,754,773,797,813,820,820,835,850,880,895,910,925,963,982,1021,1026,1029,1070,1080,1090,1100,1115,1121,1130,1148,1155,1174,1198,1214,1221,1239,1254,1269,1299,1314,1329,1344,1382,1401,1440,1440,1455,1470,1500,1515,1530,1545,1583,1602,1641,1646,1649,1690,1700,1710,1720,1735,1741,1750,1768,1775,1794,1818,1834,1841,1859,1874,1889,1919,1934,1949,1964,2002,2021,2060,2065,2068,2109,2119,2129,2139,2154,2160]

ys :: [Double]
ys = [1.0,6.0,4.0,2.0,1.0,5.0,5.0,2.0,3.0,5.0,6.0,4.0,5.0,6.0,1.0,1.0,5.0,2.0,4.0,5.0,6.0,7.0,41.0,1.0,5.0,1.0,2.0,4.0,5.0,15.0,2.0,4.0,1.0,4.0,1.0,2.0,4.0,1.0,2.0,4.0,5.0,6.0,6.0,4.0,3.0,3.0,4.0,5.0,1.0,2.0,4.0,5.0,2.0,3.0,3.0,2.0,1.0,4.0,5.0,6.0,1.0,3.0,4.0,2.0,4.0,2.0,1.0,414.0,3.0,13.0,4.0,1.0,3.0,5.0,53.0,12.0,1.0,2.0,4.0,5.0,1.0,3.0,1.0,2.0,2.0,6.0,4.0,3.0,5.0,3.0,2.0,1.0,5.0,124.0,5.0,4.0,23.0,5.0,5.0,1.0,2.0,5.0,4.0,23.0,4.0,5.0,2.0,3.0,4.0,23.0,4.0,23.0,23.0,23.0,2.0,3.0,33.0,43.0,43.0,34.0,12.0,43.0,21.0,123.0,12.0,32.0,32.0,32.0]

fromL :: [(Int, Double)] -> [TVNoKey]
fromL = map (\(t, v) -> TVNoKey t v)

tvnkl :: [TVNoKey]
tvnkl = tvnkFromList $ zip xs ys

createFalseWavelet :: [TVNoKey] -> WaveletTransformed Double
createFalseWavelet tvList = waveletFromList rep start end sampleRate
  where start = minimum timeList
        end = maximum timeList
        timeList = tvNkSimpleTime <$> tvList
        valList = tvNkSimpleValue <$> tvList
        (rep, sampleRate) = createSampledList start end tvList

createSampledList :: Int -> Int -> [TVNoKey] -> ([Double], Int)
createSampledList start end list = (rep,rate)
  where rate = findSampleRate mesh
        mesh = createMesh impls
        impls = transformImpulse list
        valList = tvNkSimpleValue <$> list
        timeList = tvNkSimpleTime <$> list
        linear = tabulateAndCreate list
        rep = snd . unzip $ sampleLinearInterp start end rate linear


sampleLinearInterp :: Int -> Int -> Int -> LinearInterp (ImpulseMesh Double) -> [(Int,Double)]
sampleLinearInterp start end sampleRate linear = map (\x -> (x, at linear $ fromIntegral x)) [start,start+sampleRate..end]

findSampleRate :: ImpulseMesh Double -> Int
findSampleRate mesh = max 15 $ truncate (maxDelta / 2.1)
  where maxDelta = V.foldl1' (\a b -> min a b) rep
        rep = impulseMeshRep mesh

data ImpulseTransformed = ImpulseTransformed {
    impulseRepresentation :: S.Seq TVNoKey
  , impulseStart          :: Int
  , impulseEnd            :: Int
} deriving (Show, Ord, Eq, Typeable)


instance Bound (ImpulseTransformed) where
  bounds = impulseBounds

instance Queryable (ImpulseTransformed) TVNoKey where
  query step start end impls = I.toInsertable $ queryImpulse impls step start end

data ImpulseMesh a  = ImpulseMesh {
  impulseMeshRep :: V.Vector a
, impulseMeshLower :: Double
, impulseMeshUpper :: Double
} deriving (Show, Ord, Eq, Typeable)


instance Indexable (ImpulseMesh a) where
  type IndexVal (ImpulseMesh a) = a
  size = size . impulseMeshRep
  unsafeIndex = unsafeIndex . impulseMeshRep

instance Mesh (ImpulseMesh Double) where
  meshLowerBound = impulseMeshLower
  meshUpperBound = impulseMeshUpper
  meshFindIndex = findIndex


findIndex :: ImpulseMesh Double -> Double -> Int
findIndex mesh x = V.maximum . V.findIndices (\a -> a <= x) $ impulseMeshRep mesh


tvnkFromList :: [(Int, Double)] -> [TVNoKey]
tvnkFromList l = (\(t ,v) -> TVNoKey t v) <$> l

tabulateAndCreate :: [TVNoKey] -> LinearInterp (ImpulseMesh Double)
tabulateAndCreate tvnklist = linearInterp $ tabulate mesh dVec
  where dVec = V.fromList . F.toList . fmap (tvNkSimpleValue) $ impulseRepresentation tf
        mesh = createMesh tf
        tf = transformImpulse tvnklist

createMesh :: ImpulseTransformed -> ImpulseMesh Double
createMesh tf = ImpulseMesh rep (mn) (mx)
  where rep = V.fromList . F.toList $ ( fromIntegral . tvNkSimpleTime <$> (impulseRepresentation tf) )
        (mn,mx) = minMax rep

reconstructImpulse :: (ImpulseTransformed) -> [TVNoKey]
reconstructImpulse = F.toList . impulseRepresentation

queryImpulse :: (ImpulseTransformed) -> Int -> Int -> Int -> S.Seq TVNoKey
queryImpulse tf step start end = trim . impulseRepresentation $ tf
  where trim = (S.dropWhileL (\x -> (tvNkSimpleTime x) >= start)) . (S.dropWhileR (\x -> (tvNkSimpleTime x) <= end))

transformImpulse :: [TVNoKey] -> (ImpulseTransformed)
transformImpulse tvnklist = ImpulseTransformed rep start end
  where rep = (S.unstableSortBy (compare `on` tvNkSimpleTime)) . S.fromList $ tvnklist -- Ensure that the list is sorted on time
        start = tvNkSimpleTime . headSeq $ rep
        end = tvNkSimpleTime . lastSeq $ rep


impulseBounds :: (ImpulseTransformed) -> (Int, Int)
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

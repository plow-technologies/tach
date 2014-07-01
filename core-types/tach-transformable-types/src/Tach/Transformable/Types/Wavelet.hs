{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tach.Transformable.Types.Wavelet where

import           Control.Applicative
import qualified Data.Foldable                as F
import qualified Data.Sequence                as S
import           Data.Wavelets.Construction
import           Data.Wavelets.Reconstruction
import           Data.Wavelets.Scaling
import           GHC.Generics
import           Tach.Class.Bounds
import qualified Tach.Class.Insertable        as I
import           Tach.Class.Queryable
import           Tach.Impulse.Types.TimeValue
import           Tach.Periodic

data WaveletTransformed a = WaveletTransformed {
    waveletStart          :: Int
  , waveletEnd            :: Int
  , waveletRepresentation :: [[a]]
  , waveletScaling        :: OldSeriesFactors
  , waveletDelta          :: Int
  , waveletLevels         :: Int
  } deriving (Show, Eq, Ord)

instance (Ord a) => Bound (WaveletTransformed a) where
  bounds = waveletBounds

instance Queryable (WaveletTransformed Double) TVNoKey where
  query step start end wvlt = I.toInsertable $ queryWavelet wvlt step start end


reconstructWavelet :: WaveletTransformed Double -> [TVNoKey]
reconstructWavelet transformed = zipWith TVNoKey times untransformed
  where untransformed = reconstructHaarTimeSeries levels $ waveletRepresentation transformed
        levels = waveletLevels transformed
        times = [(waveletStart transformed),(waveletStart transformed)+(waveletDelta transformed)..]

queryWavelet :: WaveletTransformed Double -> Int -> Int -> Int -> [TVNoKey]
queryWavelet transformed step start end = zipWith TVNoKey times $ scale untransformed
  where untransformed = reconstructHaarTimeSeries level $ waveletRepresentation transformed
        level = calcLevels transformed start end step
        nsf = NSF . computeSeriesFactors $ untransformed
        scale = applyScalingMatrix (computeScalingMatrix nsf $ waveletScaling transformed)
        times = [(waveletStart transformed),(waveletStart transformed)+(waveletDelta transformed)..]



transformWavelet :: [TVNoKey] -> [(Either (S.Seq TVNoKey) (WaveletTransformed Double))]
transformWavelet tvnklist =
  let classified = tvDataToEither <$> classifyData 15 1 200 tvNkSimpleTime tvnklist
  in (fmap (transformClassified 15)) <$> classified

transformClassified :: Int -> PeriodicData TVNoKey -> WaveletTransformed Double
transformClassified delta periodic =
  let periodicList = F.toList . unPeriodicData $ periodic
      start = tvNkSimpleTime . head $ periodicList
      end = tvNkSimpleTime . last $ periodicList
  in waveletFromList (tvNkSimpleValue <$> periodicList) start end delta



waveletFromList :: [Double] -> Int -> Int -> Int -> WaveletTransformed Double
waveletFromList rep start end delta = WaveletTransformed start end newRep scale delta levels
  where newRep = defaultVdwt levels rep
        levels = ceiling ( logBase 2 (fromIntegral . length $ rep))
        scale = OSF . computeSeriesFactors $ rep

waveletBounds :: WaveletTransformed a -> (Int, Int)
waveletBounds wvlt = (waveletStart wvlt, waveletEnd wvlt)


calcLevels :: WaveletTransformed Double -> Int -> Int -> Int -> Int
calcLevels transformed start end step
  | start > waveletStart transformed || end < waveletEnd transformed  = waveletLevels transformed
  | end - start > (waveletEnd transformed) - (waveletStart transformed) = 1
  | otherwise = ceiling $ ((fromIntegral ((waveletEnd transformed) - (waveletStart transformed)) / td))
    where td :: Double
          td = logBase 2.0 (fromIntegral step)


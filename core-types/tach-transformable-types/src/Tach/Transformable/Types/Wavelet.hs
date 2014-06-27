{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tach.Transformable.Types.Wavelet where

import           Control.Applicative
import qualified Data.Foldable                as F
import qualified Data.Sequence                as S
import           Data.Wavelets.Construction
import           Data.Wavelets.Reconstruction
import           Data.Wavelets.Scaling
import           GHC.Generics
import           Tach.Periodic

data WaveletTransformed a = WaveletTransformed {
    waveletScaling        :: OldSeriesFactors
  , waveletRepresentation :: [[a]]
  , waveletStart          :: Int
  , waveletEnd            :: Int
  , waveletDelta          :: Int
  , waveletLevels         :: Int
  } deriving (Show)

reconstructWavelet :: WaveletTransformed Double -> [(Int, Double)]
reconstructWavelet transformed = zip times untransformed
  where untransformed = reconstructHaarTimeSeries levels $ waveletRepresentation transformed
        levels = waveletLevels transformed
        times = [(waveletStart transformed),(waveletStart transformed)+(waveletDelta transformed)..]

queryWavelet :: WaveletTransformed Double -> Int -> Int -> Int -> [Double]
queryWavelet transformed step start end = scale untransformed
  where untransformed = reconstructHaarTimeSeries level $ waveletRepresentation transformed
        level = calcLevels transformed start end step
        nsf = NSF . computeSeriesFactors $ untransformed
        scale = applyScalingMatrix (computeScalingMatrix nsf $ waveletScaling transformed)


transformWavelet :: [(Int, Double)] -> [(Either (S.Seq (Int, Double)) (WaveletTransformed Double))]
transformWavelet tvnklist =
  let classified = tvDataToEither <$> classifyData 15 1 200 fst tvnklist
  in (fmap (transformClassified 15)) <$> classified

transformClassified :: Int -> PeriodicData (Int, Double) -> WaveletTransformed Double
transformClassified delta periodic =
  let periodicList = F.toList . unPeriodicData $ periodic
      start = fst . head $ periodicList
      end = fst . last $ periodicList
  in waveletFromList (snd <$> periodicList) start end delta



waveletFromList :: [Double] -> Int -> Int -> Int -> WaveletTransformed Double
waveletFromList rep start end delta = WaveletTransformed scale newRep start end delta levels
  where newRep = defaultVdwt levels rep
        levels = ceiling ( logBase 2 (fromIntegral . length $ rep))
        scale = OSF . computeSeriesFactors $ rep

calcLevels :: WaveletTransformed Double -> Int -> Int -> Int -> Int
calcLevels transformed start end step
  | start > waveletStart transformed || end < waveletEnd transformed  = waveletLevels transformed
  | end - start > (waveletEnd transformed) - (waveletStart transformed) = 1
  | otherwise = ceiling $ ((fromIntegral ((waveletEnd transformed) - (waveletStart transformed)) / td))
    where td :: Double
          td = logBase 2.0 (fromIntegral step)


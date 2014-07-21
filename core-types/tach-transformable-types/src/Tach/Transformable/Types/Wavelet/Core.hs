{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Tach.Transformable.Types.Wavelet.Core where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import qualified Data.Foldable                     as F
import qualified Data.Sequence                     as S
import           Data.Traversable
import           Data.Typeable
import           Data.Wavelets.Construction
import           Data.Wavelets.Reconstruction
import           Data.Wavelets.Scaling
import           GHC.Generics
import           Plow.Extras.Lens
import           Tach.Class.Bounds
import qualified Tach.Class.Insertable             as I
import           Tach.Class.Queryable
import           Tach.Impulse.Types.TimeValue
import           Tach.Periodic
import           Tach.Periodic
import           Tach.Transformable.Types.Internal
import           Tach.Types.Classify

toTuple :: [TVNoKey] -> [(Int, Double)]
toTuple xs = map (\(TVNoKey t v) -> (t,v)) xs

data WaveletTransformed a = WaveletTransformed {
    waveletStart          :: Int
  , waveletEnd            :: Int
  , waveletRepresentation :: [[a]]
  , waveletScaling        :: OldSeriesFactors
  , waveletDelta          :: Int
  , waveletLevels         :: Int
} deriving (Show, Eq, Ord, Typeable)

instance (Ord a) => Bound (WaveletTransformed a) where
  bounds = waveletBounds

instance Queryable (WaveletTransformed Double) TVNoKey where
  query step start end wvlt = I.toInsertable $ queryWavelet wvlt step start end


waveletFromList :: [Double] -> Int -> Int -> Int -> WaveletTransformed Double
waveletFromList rep start end delta = WaveletTransformed start end newRep scale delta levels
  where newRep = defaultVdwt levels rep
        levels = ceiling ( logBase 2 ((fromIntegral . length $ rep) / 2))
        scale = OSF . computeSeriesFactors $ rep

-- Fully deconstruct a wavelet and scale it
reconstructWavelet :: WaveletTransformed Double -> [TVNoKey]
reconstructWavelet transformed = qScaleWvlt transformed level
  where level = waveletLevels transformed

-- Construct a wavelet by going into the wavelet in a certain amount
queryWavelet :: WaveletTransformed Double -> Int -> Int -> Int -> [TVNoKey]
queryWavelet transformed step start end = qScaleWvlt transformed level
  where level = calcLevels transformed start end step

-- Query into a wavelet at a given level and scale correctly
qScaleWvlt :: WaveletTransformed Double -> Int -> [TVNoKey]
qScaleWvlt transformed level = zipWith TVNoKey times $ scale untransformed
  where untransformed = reconstructHaarTimeSeries level $ waveletRepresentation transformed
        timeCount = length untransformed
        nsf = NSF . computeSeriesFactors $ untransformed
        scale = applyScalingMatrix (computeScalingMatrix nsf $ waveletScaling transformed)
        times = truncate <$> linSpace (fromIntegral start) (fromIntegral end) timeCount
        start = waveletStart transformed
        end = waveletEnd transformed
        delta = quot (end - start) timeCount

-- Wrapper for the Bound class
waveletBounds :: WaveletTransformed a -> (Int, Int)
waveletBounds wvlt = (waveletStart wvlt, waveletEnd wvlt)

-- Determine the number of levelswaveletFromList to query into a wavelet to get the desired step
calcLevels :: WaveletTransformed Double -> Int -> Int -> Int -> Int
calcLevels transformed start end step
  | start > waveletStart transformed || end < waveletEnd transformed  = waveletLevels transformed
  | end - start > waveletEnd transformed - waveletStart transformed = 1
  | otherwise = if (goal > maxLevel) then maxLevel else goal
    where goal = ceiling (fromIntegral (waveletEnd transformed - waveletStart transformed) / td)
          maxLevel = waveletLevels transformed
          td :: Double
          td = logBase 2.0 (fromIntegral step)

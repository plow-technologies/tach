{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Tach.Transformable.Types.Wavelet where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import qualified Data.Foldable                     as F
import qualified Data.Sequence                     as S
import           Data.Traversable
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
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Internal
import           Tach.Types.Classify

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
        nsf = NSF . computeSeriesFactors $ untransformed
        scale = applyScalingMatrix (computeScalingMatrix nsf $ waveletScaling transformed)
        times = [(waveletStart transformed),waveletStart transformed + waveletDelta transformed..]

-- Attempt to transform a a list of TVs into a wavelet. It might break the list into multiple portions to classify different parts
tfWavelet :: (F.Foldable f) => f TVNoKey -> S.Seq (Classify (WaveletTransformed Double) [TVNoKey])
tfWavelet tvnklist =
  let classified = tvDataToEither <$> classifyData 15 1 200 tvNkSimpleTime tvnklist
      transformed = fmap (transformClassified 15) <$> classified
  in (second F.toList . eitherToClassify) <$> transformed

-- Top level function for transform a list of Classified Impulsive data
-- into a list of classified Impulsive and wavelet data 
transformWavelet :: Getting (S.Seq ImpulseTransformed) a1 ImpulseTransformed 
                  -> Getting (S.Seq a) a1 a
                  -> (S.Seq (Classify (WaveletTransformed Double) [TVNoKey]) -> S.Seq (Classify a2 a))
                   -> [a1]
                    -> S.Seq (Classify a2 a)
transformWavelet toAperiodic toOthers toWvlt transformed =
  let aperiodics = catPrisms toAperiodic (S.<|) transformed
      unclassified =  catPrisms toOthers (S.<|) transformed
      classifiedWavelets = F.fold $ fmap (toWvlt . tfWavelet . reconstructImpulse) aperiodics
  in classifiedWavelets  S.>< fmap Unclassified unclassified

-- Used after data has been classified. This transforms the data into Wavelet data
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


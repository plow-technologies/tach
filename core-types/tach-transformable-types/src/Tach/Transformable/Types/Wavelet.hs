{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Tach.Transformable.Types.Wavelet where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import qualified Data.Foldable                         as F
import qualified Data.Sequence                         as S
import           Data.Traversable
import           Data.Typeable
import           Data.Wavelets.Construction
import           Data.Wavelets.Reconstruction
import           Data.Wavelets.Scaling
import           GHC.Generics
import           Plow.Extras.Lens
import           Tach.Class.Bounds
import qualified Tach.Class.Insertable                 as I
import           Tach.Class.Queryable
import           Tach.Impulse.Types.TimeValue
import           Tach.Periodic
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Internal
import           Tach.Transformable.Types.Wavelet.Core
import           Tach.Types.Classify
import qualified Tach.Transformable.Types.Wavelet.Core as Tach.Transformable.Types.Wavelet




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
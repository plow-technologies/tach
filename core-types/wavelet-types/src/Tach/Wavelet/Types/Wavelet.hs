{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Tach.Wavelet.Types.Wavelet where

import Tach.Impulse.Types.Impulse 
import Tach.Impulse.Types.TimeValueSeries 
import Data.Wavelets.Scaling
import Data.Vector.Storable -- hiding (map)
import GHC.Generics



newtype TachWaveletRep = TachWaveletRep { unTachWaveletRep :: [Vector Double]} deriving (Generic,Show)





 -- | The time series factors are a set of statistical properties
 -- defined by the untransformed time series.
 -- when reconstructing this series from a down sampled wavelet they are necessary.  

data TachWavlet = TachWavelet { 
       tachWaveletTimeSeriesFactors :: OldSeriesFactors, 
       tachWaveletRep :: TachWaveletRep
} deriving (Show,Generic)





type WavletValueSeries = ImpulseSeries TVSPeriod TVSStart TVSEnd (ImpulseRep TachWavlet)






{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Tach.Transformable.Types where

import Data.Typeable
import GHC.Generics

newtype WaveletTransform a = WaveletTransform { unWaveletTransform :: [[a]]} deriving (Generic, Typeable)

type Transform a =  WaveletTransform a

{- Notes about the Wavelet transform

... will do soon.

-}

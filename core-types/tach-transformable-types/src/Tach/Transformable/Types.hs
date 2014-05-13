{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Tach.Transformable.Types where

import Tach.Transformable.Types.Internal
import GHC.Generics

newtype WaveletTransform a = WaveletTransform { unWaveletTransform :: [[a]]} deriving (Generic)
type Transform a =  (WaveletTransform a) 

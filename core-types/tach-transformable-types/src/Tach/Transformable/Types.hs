{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Tach.Transformable.Types where

import           Data.Typeable
import           GHC.Generics
import           Tach.Transformable.Types.Internal


newtype WaveletTransform a = WaveletTransform { unWaveletTransform :: [[a]]} deriving (Generic, Typeable)
type Transform a =  (WaveletTransform a)

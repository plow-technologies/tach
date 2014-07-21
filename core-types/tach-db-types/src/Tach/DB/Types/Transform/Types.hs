{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Tach.DB.Types.Transform.Types where

import           CorePrelude
import qualified DirectedKeys.Types           as DK
import           GHC.Generics
import           Tach.Impulse.Types.TimeValue



data TransformSeries key st en rep = TransformSeries {
    transformSeriesKey   :: key
   ,transformSeriesStart :: st
   ,transformSeriesEnd   :: en
   ,transformSeriesRep   :: rep
} deriving (Generic, Typeable, Ord, Eq)



newtype TransformStart = TransformStart { unTransformStart :: Int } deriving (Eq, Show, Typeable)
newtype TransformEnd = TransformEnd { unTransformEnd :: Int } deriving (Eq, Show, Typeable)
newtype TransformKey = TransformKey { unTransformKey :: DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime } deriving (Eq, Typeable)

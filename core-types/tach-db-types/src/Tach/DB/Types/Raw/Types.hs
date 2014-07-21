{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Tach.DB.Types.Raw.Types where

import           CorePrelude
import qualified DirectedKeys.Types           as DK
import           GHC.Generics
import           Tach.Impulse.Types.TimeValue



data RawSeries key st en rep = RawSeries {
    rawSeriesKey   :: key
   ,rawSeriesStart :: st
   ,rawSeriesEnd   :: en
   ,rawSeriesRep   :: rep
} deriving (Generic, Typeable, Ord, Eq)



newtype RawStart = RawStart { unRawStart :: Int } deriving (Eq, Show, Typeable)
newtype RawEnd = RawEnd { unRawEnd :: Int } deriving (Eq, Show, Typeable)
newtype RawKey = RawKey { unRawKey :: DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime } deriving (Eq, Typeable)

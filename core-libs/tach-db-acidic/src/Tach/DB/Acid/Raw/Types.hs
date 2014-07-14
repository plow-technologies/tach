{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Tach.DB.Acid.Raw.Types where

import           CorePrelude
import           Data.SafeCopy                (base, deriveSafeCopy)
import qualified Data.Vector                  as V
import qualified DirectedKeys.Types           as DK
import           GHC.Generics
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue
import qualified Data.Set as S
{-
    Storage types for tach-db

    This just relies on impulse-series
-}

data RawSeries key st en rep = RawSeries {
    rawSeriesKey :: key
   ,rawSeriesStart :: st
   ,rawSeriesEnd :: en
   ,rawSeriesRep :: rep
} deriving (Generic, Typeable, Ord, Eq)


newtype TVSimpleRawStore = TVSimpleRawStore {
  unTVSimpleRawStore :: (ImpulseSeries (ImpulseKey (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
                            (ImpulsePeriod (V.Vector Double) Int) (ImpulseStart Int) (ImpulseEnd Int) (ImpulseRep (S.Set TVNoKey)))
} deriving (Typeable, Generic)

initialTVSimpleRawStore key = TVSimpleRawStore (ImpulseSeries (ImpulseKey key) (IPeriodConst 0) (ImpulseStart 0) (ImpulseEnd 0) (ImpulseRep S.empty))

$(deriveSafeCopy 0 'base ''DK.DirectedKeyRaw)


$(deriveSafeCopy 0 'base ''KeyTime)
$(deriveSafeCopy 0 'base ''KeyPid)
$(deriveSafeCopy 0 'base ''KeySource)
$(deriveSafeCopy 0 'base ''KeyDestination)

$(deriveSafeCopy 0 'base ''ImpulseKey)
$(deriveSafeCopy 0 'base ''ImpulseStart)
$(deriveSafeCopy 0 'base ''ImpulseEnd)
$(deriveSafeCopy 0 'base ''ImpulseRep)
$(deriveSafeCopy 0 'base ''TVSimple)
$(deriveSafeCopy 0 'base ''ImpulsePeriod)
$(deriveSafeCopy 0 'base ''ImpulseSeries)


-- Our Safecopy instance
$(deriveSafeCopy 0 'base ''TVSimpleRawStore)
$(deriveSafeCopy 0 'base ''TVNoKey)

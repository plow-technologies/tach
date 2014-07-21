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
import qualified Data.Set                     as S
import qualified DirectedKeys.Types           as DK
import           GHC.Generics
import           Tach.DB.Acid.Types
import           Tach.DB.Types.Raw.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue
{-
    Storage types for tach-db

    This just relies on impulse-series
-}


newtype TVSimpleRawStore = TVSimpleRawStore {
  unTVSimpleRawStore :: RawSeries RawKey RawStart RawEnd (S.Set TVNoKey)
} deriving (Typeable, Generic)

initialTVSimpleRawStore :: DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime -> TVSimpleRawStore
initialTVSimpleRawStore key = TVSimpleRawStore $ RawSeries (RawKey key) (RawStart 0) (RawEnd 0)  S.empty

$(deriveSafeCopy 0 'base ''ImpulseKey)
$(deriveSafeCopy 0 'base ''ImpulseStart)
$(deriveSafeCopy 0 'base ''ImpulseEnd)
$(deriveSafeCopy 0 'base ''ImpulseRep)
$(deriveSafeCopy 0 'base ''ImpulsePeriod)
$(deriveSafeCopy 0 'base ''ImpulseSeries)
$(deriveSafeCopy 0 'base ''RawSeries)
$(deriveSafeCopy 0 'base ''RawStart)
$(deriveSafeCopy 0 'base ''RawEnd)
$(deriveSafeCopy 0 'base ''RawKey)


-- Our Safecopy instance
$(deriveSafeCopy 0 'base ''TVSimpleRawStore)

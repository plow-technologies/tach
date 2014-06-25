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
import qualified DirectedKeys.Types           as DK
import           GHC.Generics
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue
{-
    Storage types for tach-db

    This just relies on impulse-series
-}
newtype TVSimpleRawStore = TVSimpleRawStore {
  unTVSimpleRawStore :: (ImpulseSeries (ImpulseKey (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
                            (ImpulsePeriod (Vector Double) Int) (ImpulseStart Int) (ImpulseEnd Int) (ImpulseRep (Set TVNoKey)))
} deriving (Typeable, Generic)

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

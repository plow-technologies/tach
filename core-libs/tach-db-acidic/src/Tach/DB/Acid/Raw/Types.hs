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


module Tach.DB.Acid.Raw.Types where

import           CorePrelude
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


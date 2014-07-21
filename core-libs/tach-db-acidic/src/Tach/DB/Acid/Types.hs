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


module Tach.DB.Acid.Types where


import           CorePrelude
import           Data.SafeCopy                (base, deriveSafeCopy)
import qualified Data.Set                     as S
import qualified DirectedKeys.Types           as DK
import           GHC.Generics
import           Tach.DB.Types.Raw.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue



{-
  Error types for tach-db acid functions
-}
newtype SuccessValue = SuccessValue { getSuccess :: ByteString} deriving (Typeable)
newtype ErrorValue = ErrorValue { unCrudsError :: CrudsError} deriving (Typeable)
data CrudsError = ErrorNotFound | ErrorOutOfBounds | ErrorInvalidRange | ErrorIncorrectKey deriving (Typeable)

$(deriveSafeCopy 0 'base ''SuccessValue)
$(deriveSafeCopy 0 'base ''ErrorValue)
$(deriveSafeCopy 0 'base ''CrudsError)
$(deriveSafeCopy 0 'base ''DK.DirectedKeyRaw)
$(deriveSafeCopy 0 'base ''KeyTime)
$(deriveSafeCopy 0 'base ''KeyPid)
$(deriveSafeCopy 0 'base ''KeySource)
$(deriveSafeCopy 0 'base ''KeyDestination)
$(deriveSafeCopy 0 'base ''TVSimple)

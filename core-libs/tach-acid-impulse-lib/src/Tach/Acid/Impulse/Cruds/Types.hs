{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
 GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Acid.Impulse.Cruds.Types where


import CorePrelude

import qualified Data.ByteString.Lazy as LB
import Data.SafeCopy        ( base, deriveSafeCopy )
newtype SuccessValue = SuccessValue { getSuccess :: ByteString} deriving (Typeable, Show)
--newtype ErrorValue = ErrorValue { getError :: ByteString} deriving (Typeable, Show)

newtype ErrorValue = ErrorValue { unCrudsError :: CrudsError} deriving (Typeable, Show)

data CrudsError = ErrorNotFound | ErrorOutOfBounds | ErrorInvalidRange | ErrorIncorrectKey deriving (Typeable, Show) 

$(deriveSafeCopy 0 'base ''SuccessValue)
$(deriveSafeCopy 0 'base ''ErrorValue)
$(deriveSafeCopy 0 'base ''CrudsError)

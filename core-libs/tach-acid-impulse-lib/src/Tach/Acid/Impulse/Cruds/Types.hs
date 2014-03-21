{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
 GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Acid.Impulse.Cruds.Types where


import CorePrelude

import qualified Data.ByteString.Lazy as LB
import Data.SafeCopy        ( base, deriveSafeCopy )
newtype SuccessValue = SuccessValue { getSuccess :: ByteString} deriving (Typeable)
newtype ErrorValue = ErrorValue { getError :: ByteString} deriving (Typeable) 

$(deriveSafeCopy 0 'base ''SuccessValue)
$(deriveSafeCopy 0 'base ''ErrorValue)

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


module Tach.DB.Acid.Types where


import           CorePrelude
import           Data.SafeCopy                (base, deriveSafeCopy)




{- 
  Error types for tach-db acid functions
-}
newtype SuccessValue = SuccessValue { getSuccess :: ByteString} deriving (Typeable)
newtype ErrorValue = ErrorValue { unCrudsError :: CrudsError} deriving (Typeable)
data CrudsError = ErrorNotFound | ErrorOutOfBounds | ErrorInvalidRange | ErrorIncorrectKey deriving (Typeable)

$(deriveSafeCopy 0 'base ''SuccessValue)
$(deriveSafeCopy 0 'base ''ErrorValue)
$(deriveSafeCopy 0 'base ''CrudsError)


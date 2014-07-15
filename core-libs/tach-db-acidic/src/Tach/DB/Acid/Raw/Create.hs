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


module Tach.DB.Acid.Raw.Create (
  insertTVSimpleRaw
, insertManyTVSimpleRaw
) where

{- General Haskell related-}
import           CorePrelude
import           Data.Either                  ()
import qualified Data.Set                     as S

{- Acid/Storage -}
import           Data.Acid

{- Tach Related -}
import           Tach.DB.Acid.Raw.Internal
import           Tach.DB.Acid.Raw.Types
import           Tach.DB.Acid.Types
import           Tach.DB.Types.Raw.Types
import           Tach.Impulse.Types.TimeValue

-- | O(log n) Inserts an item into the store and updates the bounds 
insertTVSimpleRaw :: RawKey -> TVNoKey -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
insertTVSimpleRaw key item = modifyTVRawStoreWith (S.insert item) key

-- | O(n * log n) Inserts a set of items into the store and updates the bounds 
insertManyTVSimpleRaw :: RawKey -> (S.Set TVNoKey) -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
insertManyTVSimpleRaw key items = modifyTVRawStoreWith (S.union items) key
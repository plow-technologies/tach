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


module Tach.DB.Acid.Transform.Create (
  insertTVSimpleTransform
, insertManyTVSimpleTransform
) where

{- General Haskell related-}
import           CorePrelude
import           Data.Either                     ()
import qualified Data.Set                        as S

{- Acid/Storage -}
import           Data.Acid

{- Tach Related -}
import           Tach.DB.Acid.Transform.Internal
import           Tach.DB.Acid.Transform.Types
import           Tach.DB.Acid.Types
import           Tach.DB.Types.Transform.Types
import           Tach.Impulse.Types.TimeValue

-- | O(log n) Inserts an item into the store and updates the bounds
insertTVSimpleTransform :: TransformKey -> TVNoKey -> Update TVSimpleTransformStore (Either ErrorValue SuccessValue)
insertTVSimpleTransform key item = modifyTVTransformStoreWith (S.insert item) key

-- | O(n * log n) Inserts a set of items into the store and updates the bounds
insertManyTVSimpleTransform :: TransformKey -> (S.Set TVNoKey) -> Update TVSimpleTransformStore (Either ErrorValue SuccessValue)
insertManyTVSimpleTransform key items = modifyTVTransformStoreWith (S.union items) key

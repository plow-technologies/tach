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


module Tach.DB.Acid.Transform.Delete (
  deleteTVSimpleTransform
, deleteManyTVSimpleTransform
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
import           Tach.DB.Types
import           Tach.DB.Types.Transform.Types
import           Tach.Impulse.Types.TimeValue


-- | O(log n) Removes an item from the given store and updates the bounds
deleteTVSimpleTransform :: TransformKey -> TransformedInformation -> Update TVSimpleTransformStore (Either ErrorValue SuccessValue)
deleteTVSimpleTransform key item = modifyTVTransformStoreWith (S.delete item) key

-- | O(n + m) Removes an a set from the given store and updates the bounds
deleteManyTVSimpleTransform :: TransformKey -> (S.Set TransformedInformation) -> Update TVSimpleTransformStore (Either ErrorValue SuccessValue)
deleteManyTVSimpleTransform key items = modifyTVTransformStoreWith (\s -> s `S.difference` items) key

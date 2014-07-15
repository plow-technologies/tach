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


module Tach.DB.Acid.Raw.Delete (
  deleteTVSimpleRaw
, deleteManyTVSimpleRaw
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
import           Tach.Impulse.Types.TimeValue


-- | O(log n) Removes an item from the given store and updates the bounds
deleteTVSimpleRaw :: RawKey -> TVNoKey -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
deleteTVSimpleRaw key item = modifyTVRawStoreWith (S.delete item) key

-- | O(n + m) Removes an a set from the given store and updates the bounds
deleteManyTVSimpleRaw :: RawKey -> (S.Set TVNoKey) -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
deleteManyTVSimpleRaw key items = modifyTVRawStoreWith (\s -> s `S.difference` items) key

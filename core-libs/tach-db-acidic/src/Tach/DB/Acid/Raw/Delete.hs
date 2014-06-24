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
  deleteTVSimpleImpulseRaw
, deleteManyTVSimpleImpulseRaw
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
deleteTVSimpleImpulseRaw :: TVKey -> TVNoKey -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
deleteTVSimpleImpulseRaw key item = modifyTVRawStoreWith (S.delete item) key

-- | O(n * log n) Removes an a set from the given store and updates the bounds
deleteManyTVSimpleImpulseRaw :: TVKey -> (S.Set TVNoKey) -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
deleteManyTVSimpleImpulseRaw key items = modifyTVRawStoreWith (S.difference items) key
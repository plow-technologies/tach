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


module Tach.DB.Acid.Raw.Internal where


{- General Haskell related-}
import           Control.Lens                 (over, set, view, views)
import           Control.Monad.Reader         (ask)
import           Control.Monad.State.Class
import           CorePrelude
import qualified Data.ByteString.Lazy         as LB
import           Data.Either                  ()
import           Data.Set

{- Acid/Storage -}
import           Data.Acid
import           Data.Aeson

{- Tach Related -}
import           Tach.DB.Acid.Raw.Lens
import           Tach.DB.Acid.Raw.Types
import           Tach.DB.Acid.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue


-- | Generalized function to modify the underlying set of a tvstore
modifyTVRawStoreWith :: (Set TVNoKey -> Set TVNoKey) -> RawKey -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
modifyTVRawStoreWith withFunc key = do
  st@(TVSimpleRawStore (RawSeries {rawSeriesKey = k})) <- get
  case st of
    _
      | k == key -> put newSt >> (return . Right. SuccessValue . LB.toStrict . encode . object $ ["setSize" .= sz])
      | otherwise -> return . Left . ErrorValue $ ErrorIncorrectKey
            where (newSt, sz) = updateTVRawStoreWith st withFunc

-- | Only when the key is correct this is used to modify the store and return a new store after
-- applying the function and updating the bounds
updateTVRawStoreWith :: TVSimpleRawStore -> (Set TVNoKey -> Set TVNoKey) -> (TVSimpleRawStore, Int)
updateTVRawStoreWith store withFunc = (st'', sz)
  where st' = over _unTVSimpleRawStore insertTimeValue store
        sz = views _TVSimpleImpulseRep size st'
        insertTimeValue = (over (_rawSeriesRep ) withFunc)
        st'' = (over _unTVSimpleRawStore (updateLower . updateHigher) st')
        newSet = (view (_unTVSimpleRawStore . _rawSeriesRep ) st')
        updateHigher = (set (_rawSeriesEnd . _unRawEnd) (tvNkSimpleTime $ findMax newSet) )
        updateLower = (set (_rawSeriesStart . _unRawStart) (tvNkSimpleTime $ findMin newSet) )


-- | Generalized function to read from a store
readFromTVRawStoreWith :: RawKey -> (Set TVNoKey -> Either ErrorValue b) -> Query TVSimpleRawStore (Either ErrorValue b)
readFromTVRawStoreWith key withFunc = queryFcn <$> ask
  where queryFcn st
          |(isKey st) = views _TVSimpleImpulseRep withFunc st
          |otherwise   = Left $ ErrorValue ErrorIncorrectKey
        isKey (TVSimpleRawStore (RawSeries {rawSeriesKey = k})) = k == key

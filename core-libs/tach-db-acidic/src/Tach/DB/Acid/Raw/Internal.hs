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


modifyTVRawStoreWith :: (Set TVNoKey -> Set TVNoKey) -> TVKey -> Update TVSimpleRawStore (Either ErrorValue SuccessValue)
modifyTVRawStoreWith withFunc key = do
  st@(TVSimpleRawStore (ImpulseSeries {impulseSeriesKey = k})) <- get
  case st of
    _
      | k == key -> put newSt >> (return . Right. SuccessValue . LB.toStrict . encode . object $ ["setSize" .= sz])
      | otherwise -> return . Left . ErrorValue $ ErrorIncorrectKey
            where (newSt, sz) = updateTVRawStoreWith st withFunc

updateTVRawStoreWith :: TVSimpleRawStore -> (Set TVNoKey -> Set TVNoKey) -> (TVSimpleRawStore, Int)
updateTVRawStoreWith store withFunc = (st'', sz)
  where st' = over _unTVSimpleRawStore insertTimeValue store
        sz = views _TVSimpleImpulseRep size st'
        insertTimeValue = (over (_impulseSeriesRep . _unRep ) withFunc)
        st'' = (over _unTVSimpleRawStore (updateLower . updateHigher) st')
        newSet = (view (_unTVSimpleRawStore . _impulseSeriesRep . _unRep) st')
        updateHigher = (set (_impulseSeriesEnd . _unEnd) (tvNkSimpleTime $ findMax newSet) )
        updateLower = (set (_impulseSeriesStart . _unStart) (tvNkSimpleTime $ findMin newSet) )

readFromTVRawStoreWith :: TVKey -> (Set TVNoKey -> Either ErrorValue b) -> Query TVSimpleRawStore (Either ErrorValue b)
readFromTVRawStoreWith key withFunc = queryFcn <$> ask
  where queryFcn st
          |(isKey st) = views _TVSimpleImpulseRep withFunc st
          |otherwise   = Left $ ErrorValue ErrorIncorrectKey
        isKey (TVSimpleRawStore (ImpulseSeries {impulseSeriesKey = k})) = k == key

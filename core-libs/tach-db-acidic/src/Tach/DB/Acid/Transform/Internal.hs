{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
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

module Tach.DB.Acid.Transform.Internal where






{- General Haskell related-}
import           Control.Lens                 (over, set, view, views)
import           Control.Monad.Reader         (ask)
import           Control.Monad.State.Class
import           CorePrelude
import qualified Data.ByteString.Lazy         as LB
import           Data.Either                  ()
import qualified Data.Set                     as S

{- Acid/Storage -}
import           Data.Acid
import           Data.Aeson

{- Tach Related -}
import           Tach.DB.Acid.Transform.Lens
import           Tach.DB.Acid.Transform.Types
import           Tach.DB.Acid.Types
import           Tach.DB.Types.Transform.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue



-- | Generalized function to modify the underlying set of a tvstore
modifyTVTransformStoreWith :: (S.Set TVNoKey -> S.Set TVNoKey) -> TransformKey -> Update TVSimpleTransformStore (Either ErrorValue SuccessValue)
modifyTVTransformStoreWith withFunc key = do
  st@(TVSimpleTransformStore (TransformSeries {transformSeriesKey = k})) <- get
  case st of
    _
      | k == key -> put newSt >> (return . Right. SuccessValue . LB.toStrict . encode . object $ ["setSize" .= sz])
      | otherwise -> return . Left . ErrorValue $ ErrorIncorrectKey
            where (newSt, sz) = updateTVTransformStoreWith st withFunc

-- | Only when the key is correct this is used to modify the store and return a new store after
-- applying the function and updating the bounds
updateTVTransformStoreWith :: TVSimpleTransformStore -> (S.Set TVNoKey -> S.Set TVNoKey) -> (TVSimpleTransformStore, Int)
updateTVTransformStoreWith store withFunc = (st'', sz)
  where st' = over _unTVSimpleTransformStore insertTimeValue store
        sz = views _TVSimpleTransformRep S.size st'
        insertTimeValue = (over (_transformSeriesRep ) withFunc)
        st'' = (over _unTVSimpleTransformStore (updateLower . updateHigher) st')
        newSet = (view (_unTVSimpleTransformStore . _transformSeriesRep ) st')
        updateHigher = (set (_transformSeriesEnd . _unTransformEnd) (tvNkSimpleTime $ S.findMax newSet) )
        updateLower = (set (_transformSeriesStart . _unTransformStart) (tvNkSimpleTime $ S.findMin newSet) )


-- | Generalized function to read from a store
-- Correct key is assumed and should be checked first.
readFromTVTransformStoreWith :: TransformKey -> (Set TVNoKey -> Either ErrorValue b) -> Query TVSimpleTransformStore (Either ErrorValue b)
readFromTVTransformStoreWith key withFunc = queryFcn <$> ask
  where queryFcn st
          |(isKey st) = views _TVSimpleTransformRep withFunc st
          |otherwise   = Left $ ErrorValue ErrorIncorrectKey
        isKey (TVSimpleTransformStore (TransformSeries {transformSeriesKey = k})) = k == key

trimOff :: Ord a => (a, a) -> S.Set a -> S.Set a
trimOff (lower,upper) = trimOffLess . trimOffMore
  where
    trimOffLess s = snd $ S.split lower s
    trimOffMore s = fst $ S.split upper s

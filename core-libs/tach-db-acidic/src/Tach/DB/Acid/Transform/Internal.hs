{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tach.DB.Acid.Transform.Internal where






{- General Haskell related-}
import           Control.Lens                  (over, set, view, views)
import           Control.Monad.Reader          (ask)
import           Control.Monad.State.Class
import           CorePrelude
import qualified Data.ByteString.Lazy          as LB
import           Data.Either                   ()
import qualified Data.Set                      as S

{- Acid/Storage -}
import           Data.Acid
import           Data.Aeson

{- Tach Related -}
import           Tach.Class.Queryable
import           Tach.DB.Acid.Transform.Lens
import           Tach.DB.Acid.Transform.Types
import           Tach.DB.Acid.Types
import           Tach.DB.Types
import           Tach.DB.Types.Transform.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue



-- | Generalized function to modify the underlying set of a tvstore
modifyTVTransformStoreWith :: (S.Set TransformedInformation -> S.Set TransformedInformation) -> TransformKey -> Update TVSimpleTransformStore (Either ErrorValue SuccessValue)
modifyTVTransformStoreWith withFunc key = do
  st@(TVSimpleTransformStore (TransformSeries {transformSeriesKey = k})) <- get
  case st of
    _
      | k == key -> put newSt >> (return . Right. SuccessValue . LB.toStrict . encode . object $ ["setSize" .= sz])
      | otherwise -> return . Left . ErrorValue $ ErrorIncorrectKey
            where (newSt, sz) = updateTVTransformStoreWith st withFunc

-- | Only when the key is correct this is used to modify the store and return a new store after
-- applying the function and updating the bounds
updateTVTransformStoreWith :: TVSimpleTransformStore -> (S.Set TransformedInformation -> S.Set TransformedInformation) -> (TVSimpleTransformStore, Int)
updateTVTransformStoreWith store withFunc = (st'', sz)
  where st' = over _unTVSimpleTransformStore insertTimeValue store
        sz = views _tvSimpleTransformRep S.size st'
        insertTimeValue = (over (_transformSeriesRep ) withFunc)
        st'' = (over _unTVSimpleTransformStore (updateLower . updateHigher) st')
        newSet = (view _tvSimpleTransformRep st')
        updateHigher = (set (_transformSeriesEnd . _unTransformEnd) (snd . transformedBounds $ S.findMax newSet) )
        updateLower = (set (_transformSeriesStart . _unTransformStart) (fst . transformedBounds $ S.findMin newSet) )


-- | Generalized function to read from a store
-- Correct key is assumed and should be checked first.
readFromTVTransformStoreWith :: TransformKey -> (Set TransformedInformation -> Either ErrorValue b) -> Query TVSimpleTransformStore (Either ErrorValue b)
readFromTVTransformStoreWith key withFunc = queryFcn <$> ask
  where queryFcn st
          |(isKey st) = views _tvSimpleTransformRep withFunc st
          |otherwise   = Left $ ErrorValue ErrorIncorrectKey
        isKey (TVSimpleTransformStore (TransformSeries {transformSeriesKey = k})) = k == key

trimOff :: Ord a => (a, a) -> S.Set a -> S.Set a
trimOff (lower,upper) = trimOffLess . trimOffMore
  where
    trimOffLess s = snd $ S.split lower s
    trimOffMore s = fst $ S.split upper s

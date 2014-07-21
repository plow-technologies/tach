{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.DB.Acid.Transform.Read (
    getTVSimpleTransform
  , getManyTVSimpleTransform
  , getTVSimpleTransformSize
  , getTVSimpleTransformTimeBounds
) where

{- General Haskell related-}
import           Control.Lens                       (view)
import           Control.Monad.Reader               (ask)
import           CorePrelude
import           Data.Either                        ()
import qualified Data.Set                           as S
{- Acid/Storage -}
import           Data.Acid
{- Tach Related -}
import           Tach.DB.Acid.Transform.Internal
import           Tach.DB.Acid.Transform.Lens
import           Tach.DB.Acid.Transform.Types
import           Tach.DB.Acid.Types
import           Tach.DB.Types.Transform.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue
import           Tach.Impulse.Types.TimeValueSeries (TVSEnd, TVSStart)



getTVSimpleTransform :: TransformKey -> Int -> Query TVSimpleTransformStore (Either ErrorValue TVNoKey)
getTVSimpleTransform key time = readFromTVTransformStoreWith key (elookupLE (TVNoKey time 0.0))

getManyTVSimpleTransform :: TransformKey -> TVSStart -> TVSEnd -> Query TVSimpleTransformStore (Either ErrorValue (Set TVNoKey))
getManyTVSimpleTransform key start end
    | unStart start <= unEnd end = readFromTVTransformStoreWith key (\s -> Right . (trimOff (fakeTvNoKeyStart, fakeTvNoKeyEnd)) $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
      where
        fakeTvNoKeyStart = TVNoKey (unStart start) 0.0
        fakeTvNoKeyEnd = TVNoKey (unEnd end) 0.0

getTVSimpleTransformSize :: TransformKey -> Query TVSimpleTransformStore (Either ErrorValue Int)
getTVSimpleTransformSize key = readFromTVTransformStoreWith key (\s -> Right . S.size $ s)

getTVSimpleTransformTimeBounds :: TransformKey -> Query TVSimpleTransformStore (Either ErrorValue (TransformStart, TransformEnd ))
getTVSimpleTransformTimeBounds tk = queryFcn <$> ask
  where
    isKey (TVSimpleTransformStore (TransformSeries {transformSeriesKey = k})) = k == tk
    queryFcn st
      | (isKey st) = let tvSet = view _unTVSimpleTransformStore st
                      in Right (transformSeriesStart $ tvSet, transformSeriesEnd $ tvSet)
      | otherwise = (Left $ ErrorValue ErrorIncorrectKey)

elookupLE :: Ord a => a -> Set a -> Either ErrorValue a
elookupLE a s = case S.lookupLE a s of
                  Nothing -> Left $ ErrorValue ErrorNotFound
                  Just l -> Right l

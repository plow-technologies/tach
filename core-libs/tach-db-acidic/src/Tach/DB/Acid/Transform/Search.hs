{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}


module Tach.DB.Acid.Transform.Search (
    queryTvSimpleTransform
  , queryTvSimple
) where

{- General Haskell related-}
import           Control.Lens                          (view)
import           Control.Monad.Reader                  (ask)
import           CorePrelude
import           Data.Either                           ()
import qualified Data.Foldable                         as F
import qualified Data.Set                              as S
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
import           Tach.Impulse.Types.TimeValueSeries    (TVSEnd, TVSStart)
import           Tach.Transformable.Types.Impulse.Core


queryTvSimpleTransform :: TransformKey -> TransformStart -> TransformEnd -> Query TVSimpleTransformStore (Either ErrorValue [TVNoKey])
queryTvSimpleTransform key start end
    | unTransformStart start <= unTransformEnd end = readFromTVTransformStoreWith key (\s -> Right . F.toList . trimOff (lowerTvNoKey, upperTvNoKey) $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
    where
        lowerTvNoKey = TVNoKey (unTransformStart start) 0.0
        upperTvNoKey = TVNoKey (unTransformEnd end) 0.0

queryTvSimple :: TransformKey -> TransformStart -> TransformEnd -> Int -> Query TVSimpleTransformStore (Either ErrorValue [TVNoKey])
queryTvSimple key start end step
    | unTransformStart start <= unTransformEnd end = readFromTVTransformStoreWith key (\s -> Right . querySet . trimOff (lowerTvNoKey, upperTvNoKey) $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
    where
        lowerTvNoKey = TVNoKey (unTransformStart start) 0.0
        upperTvNoKey = TVNoKey (unTransformEnd end) 0.0
        querySet = (\interp -> queryLinearIterpSmooth interp step (unTransformStart start) (unTransformEnd end)) . createLinearInterp . F.toList

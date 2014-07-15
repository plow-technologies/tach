{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Tach.DB.Acid.Raw.Search (
    queryTvSimpleRaw
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
import           Tach.DB.Acid.Raw.Internal
import           Tach.DB.Acid.Raw.Lens
import           Tach.DB.Acid.Raw.Types
import           Tach.DB.Acid.Types
import           Tach.DB.Types.Raw.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue
import           Tach.Impulse.Types.TimeValueSeries    (TVSEnd, TVSStart)
import           Tach.Transformable.Types.Impulse.Core


queryTvSimpleRaw :: RawKey -> RawStart -> RawEnd -> Query TVSimpleRawStore (Either ErrorValue [TVNoKey])
queryTvSimpleRaw key start end
    | unRawStart start <= unRawEnd end = readFromTVRawStoreWith key (\s -> Right . F.toList . trimOff (lowerTvNoKey, upperTvNoKey) $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
    where
        lowerTvNoKey = TVNoKey (unRawStart start) 0.0
        upperTvNoKey = TVNoKey (unRawEnd end) 0.0

queryTvSimple :: RawKey -> RawStart -> RawEnd -> Int -> Query TVSimpleRawStore (Either ErrorValue [TVNoKey])
queryTvSimple key start end step
    | unRawStart start <= unRawEnd end = readFromTVRawStoreWith key (\s -> Right . querySet . trimOff (lowerTvNoKey, upperTvNoKey) $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
    where
        lowerTvNoKey = TVNoKey (unRawStart start) 0.0
        upperTvNoKey = TVNoKey (unRawEnd end) 0.0
        querySet = (\interp -> queryLinearIterpSmooth interp step (unRawStart start) (unRawEnd end)) . createLinearInterp . F.toList

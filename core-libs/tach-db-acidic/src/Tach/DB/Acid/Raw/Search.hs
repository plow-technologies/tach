{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Tach.DB.Acid.Raw.Searc (
    queryTvSimpleImpulseRaw
  , queryTvSimpleImpulse
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
import           Tach.DB.Acid.Raw.Internal
import           Tach.DB.Acid.Raw.Lens
import           Tach.DB.Acid.Raw.Types
import           Tach.DB.Acid.Types
import           Tach.Impulse.Types.Impulse
import           Tach.Impulse.Types.TimeValue
import           Tach.Impulse.Types.TimeValueSeries (TVSEnd, TVSStart)



getTVSimpleImpulseRaw :: RawKey -> Int -> Query TVSimpleRawStore (Either ErrorValue TVNoKey)
getTVSimpleImpulseRaw key time = readFromTVRawStoreWith key (elookupLE (TVNoKey time 0.0))

getManyTVSimpleImpulseRaw :: RawKey -> TVSStart -> TVSEnd -> Query TVSimpleRawStore (Either ErrorValue (Set TVNoKey))
getManyTVSimpleImpulseRaw key start end
    | unStart start <= unEnd end = readFromTVRawStoreWith key (\s -> Right . trimOff $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
      where
        fakeTvNoKeyStart = TVNoKey (unStart start) 0.0
        fakeTvNoKeyEnd = TVNoKey (unEnd end) 0.0
        trimOff = trimOffLess . trimOffMore
        trimOffLess s = snd $ S.split fakeTvNoKeyStart s
        trimOffMore s = fst $ S.split fakeTvNoKeyEnd s

getTVSimpleImpulseRawSize :: RawKey -> Query TVSimpleRawStore (Either ErrorValue Int)
getTVSimpleImpulseRawSize key = readFromTVRawStoreWith key (\s -> Right . S.size $ s)

getTVSimpleImpulseRawTimeBounds :: RawKey -> Query TVSimpleRawStore (Either ErrorValue (RawStart, RawEnd ))
getTVSimpleImpulseRawTimeBounds tk = queryFcn <$> ask
  where
    isKey (TVSimpleRawStore (RawSeries {rawSeriesKey = k})) = k == tk
    queryFcn st
      | (isKey st) = let tvSet = view _unTVSimpleRawStore st
                      in Right (rawSeriesStart tvSet, rawSeriesEnd tvSet)
      | otherwise = (Left $ ErrorValue ErrorIncorrectKey)

elookupLE :: Ord a => a -> Set a -> Either ErrorValue a
elookupLE a s = case S.lookupLE a s of
                  Nothing -> Left $ ErrorValue ErrorNotFound
                  Just l -> Right l

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Tach.DB.Acid.Raw.Read (
    getTVSimpleRaw
  , getManyTVSimpleRaw
  , getTVSimpleRawSize
  , getTVSimpleRawTimeBounds
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



getTVSimpleRaw :: RawKey -> Int -> Query TVSimpleRawStore (Either ErrorValue TVNoKey)
getTVSimpleRaw key time = readFromTVRawStoreWith key (elookupLE (TVNoKey time 0.0))

getManyTVSimpleRaw :: RawKey -> TVSStart -> TVSEnd -> Query TVSimpleRawStore (Either ErrorValue (Set TVNoKey))
getManyTVSimpleRaw key start end
    | unStart start <= unEnd end = readFromTVRawStoreWith key (\s -> Right . (trimOff (fakeTvNoKeyStart, fakeTvNoKeyEnd)) $ s)
    | otherwise = return . Left $ ErrorValue ErrorInvalidRange
      where
        fakeTvNoKeyStart = TVNoKey (unStart start) 0.0
        fakeTvNoKeyEnd = TVNoKey (unEnd end) 0.0

getTVSimpleRawSize :: RawKey -> Query TVSimpleRawStore (Either ErrorValue Int)
getTVSimpleRawSize key = readFromTVRawStoreWith key (\s -> Right . S.size $ s)

getTVSimpleRawTimeBounds :: RawKey -> Query TVSimpleRawStore (Either ErrorValue (RawStart, RawEnd ))
getTVSimpleRawTimeBounds tk = queryFcn <$> ask
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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Tach.Class.Bounds where

import           Control.Applicative
import           Data.Either         ()
import           Data.Function
import           Data.List
import qualified Data.Sequence       as S
import           Tach.Types.Classify
import qualified Data.Foldable as F
import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable

class (Ord a) => Bound a where
  bounds :: a -> (Int, Int)

instance (Bound a) => Bound (S.Seq a) where
  bounds aSeq = (fst $ minimumBy (compare `on` fst) list,snd $ maximumBy (compare `on` snd) list)
    where list = bounds <$> (F.toList aSeq)

instance (Bound a, F.Foldable f, Monoid (f a), Ord (f a))  => Bound (f a) where
  bounds xs =  F.foldl (\old x -> (updateYounger old) . (updateOlder old) $ (bounds x)) (-1,-1) xs
    where updateOlder :: (Int,Int) -> (Int,Int) -> (Int, Int)
          updateOlder a@(_,b) c@(_,d) = if b > d then a else c
          updateYounger :: (Int, Int) -> (Int, Int) -> (Int, Int)
          updateYounger a@(b,_) c@(d,_) = if b < d then a else c

instance (Bound a, Ord a, Bound b, Ord b, Bifunctor bf, Bifoldable bf, Ord (bf a b)) => Bound (bf a b) where
  bounds cl = bifoldr (\a _ -> bounds a)  (\a _ -> bounds a) (-1,-1) cl

boundsOfClass :: (Ord a, Ord c, Bound c, Bound a) => Classify a c -> (Int, Int)
boundsOfClass cl = combineClassify $ bimap bounds bounds cl
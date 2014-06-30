{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances #-}
module Tach.Class.Bounds where

import           Control.Applicative
import           Data.Either         ()
import           Data.Function
import           Data.List
import qualified Data.Sequence       as S
import qualified Data.Foldable as F
import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable

class Bound a where
  bounds :: a -> (Int, Int)

instance (Bound a) => Bound (S.Seq a) where
  bounds aSeq = (fst $ minimumBy (compare `on` fst) list,snd $ maximumBy (compare `on` snd) list)
    where list = bounds <$> (F.toList aSeq)

instance (Bound a, F.Foldable f, Monoid (f a))  => Bound (f a) where
  bounds xs =  F.foldl (\old x -> (updateBounds old) $ (bounds x)) (-1,-1) xs
    where updateBounds :: (Int,Int) -> (Int, Int) -> (Int, Int)
          updateBounds bs = (updateOlder bs) . (updateYounger bs)
          updateOlder :: (Int,Int) -> (Int,Int) -> (Int, Int)
          updateOlder a@(_,b) c@(_,d) = if b > d then a else c
          updateYounger :: (Int, Int) -> (Int, Int) -> (Int, Int)
          updateYounger a@(b,_) c@(d,_) = if b < d then a else c

instance (Bound a, Bound b, Bifunctor bf, Bifoldable bf) => Bound (bf a b) where
  bounds cl = bifoldr (\x old -> updateBounds old (bounds x)) (\x old -> updateBounds old (bounds x)) (-1,-1) cl
    where updateBounds :: (Int,Int) -> (Int, Int) -> (Int, Int)
          updateBounds bs = (updateOlder bs) . (updateYounger bs)
          updateOlder :: (Int,Int) -> (Int,Int) -> (Int, Int)
          updateOlder a@(_,b) c@(_,d) = if b > d then a else c
          updateYounger :: (Int, Int) -> (Int, Int) -> (Int, Int)
          updateYounger a@(b,_) c@(d,_) = if b < d then a else c
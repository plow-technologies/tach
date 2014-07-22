{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Tach.Class.Bounds where

import           Control.Applicative
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Either         ()
import qualified Data.Foldable       as F
import           Data.Function
import           Data.List
import           Data.Monoid
import qualified Data.Sequence       as S
import qualified Data.Set            as S

class Bound a where
  bounds :: a -> (Int, Int)

instance Bound () where
    bounds _ = (0,0)

instance (Bound a) => Bound [a] where
  bounds = genericBounds

instance (Bound a) => Bound (S.Set a) where
  bounds = genericBounds

genericBounds :: (Bound a, F.Foldable f) => f a -> (Int, Int)
genericBounds xs =  F.foldl (\old x -> (updateBounds old) $ (bounds x)) (-1,-1) xs
    where updateBounds :: (Int,Int) -> (Int, Int) -> (Int, Int)
          updateBounds bs = (updateOlder bs) . (updateYounger bs)
          updateOlder :: (Int,Int) -> (Int,Int) -> (Int, Int)
          updateOlder a@(_,b) c@(_,d) = if b > d then a else c
          updateYounger :: (Int, Int) -> (Int, Int) -> (Int, Int)
          updateYounger a@(b,_) c@(d,_) = if b < d then a else c
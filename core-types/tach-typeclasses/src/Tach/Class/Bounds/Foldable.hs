{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances #-}
module Tach.Class.Bounds.Foldable where

import           Data.Either         ()
import qualified Data.Foldable as F
import           Tach.Class.Bounds


instance (Bound a, F.Foldable f)  => Bound (f a) where
  bounds xs =  F.foldl (\old x -> (updateBounds old) $ (bounds x)) (-1,-1) xs
    where updateBounds :: (Int,Int) -> (Int, Int) -> (Int, Int)
          updateBounds bs = (updateOlder bs) . (updateYounger bs)
          updateOlder :: (Int,Int) -> (Int,Int) -> (Int, Int)
          updateOlder a@(_,b) c@(_,d) = if b > d then a else c
          updateYounger :: (Int, Int) -> (Int, Int) -> (Int, Int)
          updateYounger a@(b,_) c@(d,_) = if b < d then a else c
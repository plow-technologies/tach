{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Tach.Class.Bounds.Bifoldable where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Either          ()
import           Tach.Class.Bounds
import           Tach.Class.Queryable
import           Tach.Types.Classify

--instance (Bound a, Bound b, Bifunctor bf, Bifoldable bf) => Bound (bf a b) where
--  bounds cl = bifoldr (\x old -> updateBounds old (bounds x)) (\x old -> updateBounds old (bounds x)) (-1,-1) cl
--    where updateBounds :: (Int,Int) -> (Int, Int) -> (Int, Int)
--          updateBounds bs = (updateOlder bs) . (updateYounger bs)
--          updateOlder :: (Int,Int) -> (Int,Int) -> (Int, Int)
--          updateOlder a@(_,b) c@(_,d) = if b > d then a else c
--          updateYounger :: (Int, Int) -> (Int, Int) -> (Int, Int)
--          updateYounger a@(b,_) c@(d,_) = if b < d then a else c

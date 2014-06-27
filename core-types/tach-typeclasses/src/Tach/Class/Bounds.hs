{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Tach.Class.Bounds where

import           Data.Either         ()
import           Tach.Types.Classify


class (Ord a) => Bound a where
  bounds :: a -> (Int, Int)


instance (Bound a, Ord a, Bound b, Ord b) => Bound (Classify a b) where
  bounds (Unclassified l) = bounds l
  bounds (Classified r) = bounds r


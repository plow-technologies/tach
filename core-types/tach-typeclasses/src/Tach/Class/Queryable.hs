{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Tach.Class.Queryable where

import           Data.Either         ()
import           Data.Foldable
import           Tach.Types.Classify
import           Tach.Class.Bounds


class (Bound a, Ord a, Ord b) => Queryable a b where
  query :: (Num step, Num start, Num end, Foldable f, Functor f) => step -> start -> end -> a -> f b
  query = undefined


instance (Bound a, Queryable a c, Queryable b c) => Queryable (Classify a b) c where
  query step start end (Unclassified l) = query step start end l
  query step start end (Classified r) = query step start end r
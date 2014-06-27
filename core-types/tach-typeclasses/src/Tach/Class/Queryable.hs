{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Class.Queryable where

import           Data.Either         ()
import           Data.Foldable
import           Data.Sequence
import           Tach.Class.Bounds
import           Tach.Types.Classify


class (Bound a, Ord a, Ord b) => Queryable a b where
  query :: Int -> Int -> Int -> a -> Seq b
  query = undefined


instance (Bound a, Queryable a c, Queryable b c) => Queryable (Classify a b) c where
  query step start end (Unclassified l) = query step start end l
  query step start end (Classified r) = query step start end r

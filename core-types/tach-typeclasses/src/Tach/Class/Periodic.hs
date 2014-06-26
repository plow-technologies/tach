{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Class.Periodic where

import           Data.Either          ()
import           Data.Foldable
import           Tach.Class.Queryable

--class (Ord a, Ord b) => Queryable a b where
--  query :: (Num step, Num start, Num end, Foldable f) => step -> start -> end -> a -> f b
--  bounds :: (Num n) => a -> ((n, n),b)


--instance (Queryable a c, Queryable b c) => Queryable (Either a b) c where
--  query step start end (Left l) = query step start end l
--  query step start end (Right r) = query step start end r
--  bounds (Left l) = bounds l
--  bounds (Right r) = bounds r

class (Queryable a b) => HasPeriodic a b where
  classifyPeriodic :: (Functor f, Foldable f) =>  a -> f (Either PeridociData a)

data PeridociData = PeridociData {}

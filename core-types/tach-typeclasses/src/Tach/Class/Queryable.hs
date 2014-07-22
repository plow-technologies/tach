{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Class.Queryable where

import           Data.Either           ()
import qualified Data.Foldable         as F
import           Data.Monoid
import qualified Data.Sequence         as SEQ
import qualified Data.Set              as S
import           Tach.Class.Bounds
import           Tach.Class.Insertable

class (Bound a, Ord a, Ord b) => Queryable a b where
  query :: (F.Foldable f, Monoid (f b), Insertable f) => Int -> Int -> Int -> a -> f b
  query = undefined


instance (Queryable a b) => Queryable [a] b where
  query = genericFoldableQuery

instance (Queryable a b) => Queryable (S.Set a) b where
  query = genericFoldableQuery
 
genericFoldableQuery :: (Queryable a b, Insertable f, Monoid (f b), F.Foldable t) => Int -> Int -> Int -> t a -> f b
genericFoldableQuery step start end xs = F.foldl (\ys x -> ys `mappend` (query step start end x)) mempty xs



instance (Ord b) => Queryable () b where
  query _ _ _ _ = mempty
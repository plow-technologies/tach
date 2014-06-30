{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Class.Queryable where

import           Data.Either                 ()
import qualified Data.Foldable as F
import qualified Data.Sequence as SEQ
import           Tach.Class.Bounds
import           Tach.Class.Insertable
import           Tach.Types.Classify
import Data.Monoid

class (Bound a, Ord a, Ord b) => Queryable a b where
  query :: (F.Foldable f, Monoid (f b)) => Int -> Int -> Int -> a -> f b
  query = undefined


instance (Bound b, Bound a, Ord c, Queryable a c, Queryable b c) => Queryable (Classify a b) c where
  query = queryClassifier

queryClassifier :: (F.Foldable f, Queryable a c, Queryable b c, Monoid (f c)) => Int -> Int -> Int -> (Classify a b) -> f c
queryClassifier step start end (Unclassified l) = query step start end l
queryClassifier step start end (Classified r) = query step start end r



instance (Queryable a b, Monoid (f b), Bound (f a), F.Foldable f) => Queryable (f a) b where
  query step start end xs = F.foldl (\ys x -> ys `mappend` (query step start end x)) mempty xs
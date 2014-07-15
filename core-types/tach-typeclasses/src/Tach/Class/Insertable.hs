{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Class.Insertable where

import           Data.Either         ()
import           Data.Foldable
import qualified Data.Sequence as SEQ
import qualified Data.Set as SET
import qualified Data.Foldable as F


class (Foldable f) => Insertable f where
  insert :: (Ord a) => f a -> a -> f a
  empty :: f a

instance Insertable (SEQ.Seq) where
  insert = (\l i -> l SEQ.|> i)
  empty = SEQ.empty

instance Insertable SET.Set where
  insert = flip SET.insert
  empty = SET.empty

instance Insertable [] where
  insert xs x = xs ++ [x]
  empty = []

toInsertable :: (Insertable i, F.Foldable f, Ord a) => f a -> i a
toInsertable xs = F.foldl (\b a -> insert b a) empty xs
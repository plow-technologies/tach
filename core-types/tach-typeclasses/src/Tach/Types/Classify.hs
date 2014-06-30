{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Types.Classify where

import           Control.Applicative
import           Data.Bifunctor
import Data.Bifoldable


data Classify a b = Classified a | Unclassified b deriving (Show, Eq, Ord)

instance Bifunctor Classify where
  first f (Classified u) = Classified (f u)
  second g (Unclassified c) = Unclassified (g c)

instance Functor (Classify a) where
  fmap _ (Classified u) = Classified u
  fmap f (Unclassified c) = Unclassified (f c)

instance Applicative (Classify a) where
  pure = Unclassified
  Classified e <*> _ = Classified e
  Unclassified f <*> u = fmap f u

instance Monad (Classify a) where
  return = Unclassified
  Classified c >>= _ = Classified c
  Unclassified un >>= f = f un

instance Bifoldable Classify where
  bifoldMap f _ (Classified a) = f a
  bifoldMap _ g (Unclassified b) = g b

combineClassify :: Classify a a -> a
combineClassify (Classified x) = x
combineClassify (Unclassified x) = x
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Types.Classify where

import           Data.Bifunctor
import Control.Applicative

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
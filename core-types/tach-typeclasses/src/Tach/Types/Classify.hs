{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Tach.Types.Classify where

import           Control.Applicative
import           Control.Lens
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Foldable         as F
import           Data.Monoid
import           Data.Typeable
import           GHC.Generics
import           Tach.Class.Bounds
import           Tach.Class.Insertable
import           Tach.Class.Queryable

data Classify a b = Classified a | Unclassified b deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Bifunctor Classify where
  bimap f _ (Classified c) = Classified (f c)
  bimap _ g (Unclassified c) = Unclassified (g c)

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
  Unclassified uncl >>= f = f uncl

instance Bifoldable Classify where
  bifoldMap f _ (Classified a) = f a
  bifoldMap _ g (Unclassified b) = g b

instance (Bound a, Bound b) => Bound (Classify a b) where
  bounds (Classified b) = bounds b
  bounds (Unclassified b) = bounds b

instance (Queryable a c, Queryable b c) => Queryable (Classify a b) c where
  query = queryClassifier


queryClassifier :: (F.Foldable f, Queryable a c, Queryable b c, Monoid (f c), Insertable f) => Int -> Int -> Int -> (Classify a b) -> f c
queryClassifier step start end (Unclassified l) = query step start end l
queryClassifier step start end (Classified r) = query step start end r

$(makePrisms ''Classify)

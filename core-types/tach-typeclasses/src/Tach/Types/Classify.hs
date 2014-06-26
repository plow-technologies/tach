{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tach.Types.Classify where

import           Data.Bifunctor

data Classify a b = Unclassified a | Classified b deriving (Show, Eq, Ord)

instance Bifunctor Classify where
  first f (Unclassified u) = Unclassified (f u)
  second g (Classified c) = Classified (g c)

instance Functor (Classify a) where
  fmap _ (Unclassified u) = Unclassified u
  fmap f (Classified c) = Classified (f c)

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances #-}
module Tach.Class.Bounds where

import           Control.Applicative
import           Data.Either         ()
import           Data.Function
import           Data.List
import qualified Data.Sequence       as S
import qualified Data.Foldable as F
import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable

class Bound a where
  bounds :: a -> (Int, Int)

instance (Bound a) => Bound (S.Seq a) where
  bounds aSeq = (fst $ minimumBy (compare `on` fst) list,snd $ maximumBy (compare `on` snd) list)
    where list = bounds <$> (F.toList aSeq)
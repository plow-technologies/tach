
-- | Implementation of the binary transform, as detailed in
--   <>.
module Data.BinaryList.Algorithm.BinaryTransform (
    -- * Bijections
    Bijection (..)
  , inverseBijection
    -- * Binary Transform
    -- ** Left version
  , leftBinaryTransform
  , leftPartialInverse
    -- ** Right version
  -- , rightBinaryTransform
  -- , rightPartialInverse
  ) where

import Prelude hiding (id,(.))
import qualified Prelude
import Control.Category
import Data.Maybe (fromJust)
-- Binary lists
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL

-- | A bijection from @a@ to @b@ is a function from @a@ to @b@ that is invertible.
--   A function is invertible if and only if it's both injective and surjective.
--   These are the definitions of the terms /injective/ and /surjective/.
--
--   * A function @f :: a -> b@ is /injective/ if @f(x) = f(y)@ implies @x = y@
--     for every @x, y :: a@.
--
--   * A function @f :: a -> b@ is /surjective/ if for every @y :: b@ there is
--     @x :: a@ such that @f(x) = y@.
--
data Bijection a b =
  Bijection { direct  :: a -> b -- ^ Bijection.
            , inverse :: b -> a -- ^ Inverse of the bijection.
              }

{-# INLINE inverseBijection #-}

inverseBijection :: Bijection a b -> Bijection b a
inverseBijection (Bijection f f') = Bijection f' f

instance Category Bijection where
  id = Bijection Prelude.id Prelude.id
  Bijection f f' . Bijection g g' = Bijection (f Prelude.. g) (g' Prelude.. f')

-- Left Binary Transform

leftBinaryTransform :: Bijection (a,a) (a,a) -> Bijection (BinList a) (BinList a)
leftBinaryTransform (Bijection f f') = Bijection transform itransform
   where
     transform xs =
       case BL.disjoinPairs xs of
         Nothing -> xs
         Just ps -> let (l,r) = BL.unzip $ fmap f ps
                    in  fromJust $ BL.append (transform l) r
     itransform xs =
       case BL.split xs of
         Left _ -> xs
         Right (l,r) -> BL.joinPairs $ fmap f' $ BL.zip (itransform l) r

leftPartialInverse :: Bijection (BinList a) (BinList a) -> Int -> BinList a -> BinList a
leftPartialInverse (Bijection _ itransform) = go
  where
    go 0 xs = itransform xs
    go n xs =
      case BL.split xs of
        Right (l,_) -> go (n-1) l
        _ -> xs

-- Right Binary Transform

{-

rightBinaryTransform :: Bijection (a,a) (a,a) -> Bijection (BinList a) (BinList a)
rightBinaryTransform (Bijection f g) = Bijection transform itransform
   where
     transform xs =
       case BL.disjoinPairs xs of
         Nothing -> xs
         Just ps -> let (l,r) = BL.unzip $ fmap f ps
                    in  fromJust $ BL.append (transform l) r
     itransform xs =
       case BL.split xs of
         Left _ -> xs
         Right (l,r) -> BL.joinPairs $ fmap g $ BL.zip (itransform l) r

rightPartialInverse :: Bijection (BinList a) (BinList a) -> Int -> BinList a -> BinList a
rightPartialInverse (Bijection _ itransform) = go
  where
    go 0 xs = itransform xs
    go n xs =
      case BL.split xs of
        Right (_,r) -> go (n-1) r
        _ -> xs

-}

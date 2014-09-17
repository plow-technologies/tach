
-- | Implementation of the binary transform, as detailed in
--   <https://github.com/plow-technologies/writings/tree/master/binary-transform>.
module Data.BinaryList.Algorithm.BinaryTransform (
    -- * Bijections
    Bijection (..)
  , inverseBijection
  , functorBijection
    -- * Binary Transform
    -- ** Left version
  , leftBinaryTransform
  , leftInverseBinaryTransformDec
  , leftPartialInverse
    -- ** Right version
  , rightBinaryTransform
  , rightPartialInverse
  ) where

import Prelude hiding (id,(.))
import qualified Prelude
import Control.Category
import Data.Maybe (fromJust)
-- Binary lists
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL
import Data.BinaryList.Serialize (Decoded (..))

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
--   To apply a bijection @f@ to an argument @x@ use @direct f x@. To apply its
--   inverse just do @inverse f x@.
--
data Bijection a b =
  Bijection { direct  :: a -> b -- ^ Bijection.
            , inverse :: b -> a -- ^ Inverse of the bijection.
              }

{-# INLINE inverseBijection #-}

-- | The inverse of a bijection.
inverseBijection :: Bijection a b -> Bijection b a
inverseBijection (Bijection f f') = Bijection f' f

---------------------------------
---------------------------------
-- Bijections on functors

functorBijection :: Functor f => Bijection a b -> Bijection (f a) (f b)
functorBijection (Bijection f f') = Bijection (fmap f) (fmap f')

{-

Given a bijection @f :: a -> b@, with inverse @f' :: b -> a@, and a functor @c@,
we can build a bijection @g :: c a -> c b@. Namely: @g = fmap f@. The proof
goes as follows:

** Proof

Let @f :: a -> b@ be a bijection with inverse @f' :: b -> a@. Let @c@ be a functor.
Define @g = fmap f@.

* Injectivity:
     Let @v, w :: c a@ such that @g(v) = g(w)@
  by definition of g
     => fmap f v = fmap f w
  applying @fmap f'@ to both sides
     => fmap f' (fmap f v) = fmap f' (fmap f w)
  by functor law
     => fmap (f' . f) v = fmap (f' . f) w
  since f' is the inverse of f
     => fmap id v = fmap id w
  by functor law
     => v = w

* Surjectivity:
     Let @w :: c b@.
     Define @v = fmap f' w@.
     => g(v) = g (fmap f' w)
  by definition of @g@
     => g(v) = fmap f (fmap f' w)
  by functor law
     => g(v) = fmap (f . f') w
  since f' is the inverse of f
     => g(v) = fmap id w
  by functor law
     => g(v) = w

Therefore, @g :: c a -> c b@ is a bijection. Its inverse is @g' = fmap f'@. Indeed:

* Left inverse:
     Let @v :: c a@.
     => g' (g v)
  by definition of g and g'
      = fmap f' (fmap f v)
  by functor law
      = fmap (f' . f) v
  since f' is the inverse of f
      = fmap id v
  by functor law
      = v

* Right inverse:
     Let @w :: c b@.
     => g (g' w)
  by definition of g and g'
      = fmap f (fmap f' w)
  by functor law
      = fmap (f . f') w
  since f' is the inverse of f
      = fmap id w
  by functor law
      = w

-}

---------------------------------
---------------------------------

instance Category Bijection where
  id = Bijection Prelude.id Prelude.id
  Bijection f f' . Bijection g g' = Bijection (f Prelude.. g) (g' Prelude.. f')

-- Left Binary Transform

-- | The /left binary transform/ lifts a permutation (i.e. a bijection from
--   a set to itself) of a plane to a permutation of binary lists. The transformation
--   condenses at the /left/.
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

leftInverseBinaryTransformDec :: Bijection (a,a) (a,a) -> Decoded a -> Decoded a
leftInverseBinaryTransformDec (Bijection _ f') (PartialResult xs0 d0) = PartialResult xs0 $ go xs0 d0
  where
    go acc (PartialResult xs d) = 
      let ys = case BL.split xs of
                 Right (_,r) -> BL.joinPairs $ fmap f' $ BL.zip acc r
                 Left _ -> xs
      in  PartialResult ys $ go ys d
    go acc (FinalResult xs rm) =
      let ys = case BL.split xs of
                 Right (_,r) -> BL.joinPairs $ fmap f' $ BL.zip acc r
                 Left _ -> xs
      in  FinalResult ys rm
    go _ d = d
leftInverseBinaryTransformDec _ d = d

-- | Apply the inverse of a permutation of binary lists to a sublist of a binary list.
--   The 'Int' argument specifies the size of the sublist. More specifically,
--   applying @leftPartialInverse f i@ to a binary list @xs@ of length @2^n@
--   returns the result of applying @inverse f@ to the first @max{1,2^(n-i)}@ elements.
leftPartialInverse :: Bijection (BinList a) (BinList a) -> Int -> BinList a -> BinList a
leftPartialInverse t = go
  where
    go 0 xs = inverse t xs
    go n xs =
      case BL.split xs of
        Right (l,_) -> go (n-1) l
        _ -> xs

-- Right Binary Transform

-- | The /right binary transform/ lifts a permutation (i.e. a bijection from
--   a set to itself) of a plane to a permutation of binary lists. The transformation
--   condenses at the /right/.
rightBinaryTransform :: Bijection (a,a) (a,a) -> Bijection (BinList a) (BinList a)
rightBinaryTransform (Bijection f g) = Bijection transform itransform
   where
     transform xs =
       case BL.disjoinPairs xs of
         Nothing -> xs
         Just ps -> let (l,r) = BL.unzip $ fmap f ps
                    in  fromJust $ BL.append l (transform r)
     itransform xs =
       case BL.split xs of
         Left _ -> xs
         Right (l,r) -> BL.joinPairs $ fmap g $ BL.zip l (itransform r)

-- | Apply the inverse of a permutation of binary lists to a sublist of a binary list.
--   The 'Int' argument specifies the size of the sublist. More specifically,
--   applying @rightPartialInverse f i@ to a binary list @xs@ of length @2^n@
--   returns the result of applying @inverse f@ to the last @max{1,2^(n-i)}@ elements.
rightPartialInverse :: Bijection (BinList a) (BinList a) -> Int -> BinList a -> BinList a
rightPartialInverse t = go
  where
    go 0 xs = inverse t xs
    go n xs =
      case BL.split xs of
        Right (_,r) -> go (n-1) r
        _ -> xs

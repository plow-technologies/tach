
-- | Implementation of the binary transform, as detailed in
--   <https://github.com/plow-technologies/writings/tree/master/binary-transform>.
module Data.BinaryList.Algorithm.BinaryTransform (
    -- * Bijections
    Bijection (..)
  , inverseBijection
  , functorBijection
  , productBijection
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
import Control.Category
import Control.Arrow ((***))
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

{-# INLINE functorBijection #-}

-- | Lift a 'Bijection' to work over an arbitrary 'Functor'.
functorBijection :: Functor f => Bijection a b -> Bijection (f a) (f b)
functorBijection (Bijection f f') = Bijection (fmap f) (fmap f')

{-

Given a bijection @f :: a -> b@, with inverse @f' :: b -> a@, and a functor @c@,
we can build a bijection @g :: c a -> c b@. Namely: @g = fmap f@.

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
-- Product Bijection

-- | The product of two bijections. This is the equivalent to '***' for the 'Bijection' type.
productBijection :: Bijection a b -> Bijection c d -> Bijection (a,c) (b,d)
productBijection (Bijection f f') (Bijection g g') = Bijection h h'
  where
    h  = f  *** g
    h' = f' *** g'

{-

Let @f :: a -> b@ be a bijection with inverse @f' :: b -> a@.
Let @g :: c -> d@ be a bijection with inverse @g' :: d -> c@.
Define @h = f *** g@. Then @h :: (a,c) -> (b,d)@ is a bijection.

** Proof

* Injectivity:
     Let @p, q :: (a,c)@ such that @h(p) = h(q)@
  by definition of h
     => (f *** g) p = (f *** g) q
  making explicit that @p@ and @q@ are pairs
     => (f *** g) (p1,p2) = (f *** g) (q1,q2)
  by definition of (***)
     => (f p1, g p2) = (f q1, g q2)
  by pair equality
     => f p1 = f q1 AND g p2 = g q2
  by injectivy of f and g
     => p1 = q1 AND p2 = q2
  since p = (p1,p2) AND q = (q1,q2)
     => p = q

* Surjectivity:
     Let @q :: (b,d)@.
  making explicit that @q@ is a pair
     => q = (q1,q2)
        Define p = (f' q1, g' q2)
  by definition of h
     => h(p) = (f *** g) p
  by definition of p
     => h(p) = (f *** g) (f' q1, g' q2)
  by definition of (***)
     => h(p) = (f (f' q1), g (g' q2))
  since f' is the inverse of f, and g' is the inverse of g
     => h(p) = (q1,q2)
  since q = (q1,q2)
     => h(p) = q

Therefore, @h :: (a,c) -> (b,d)@ is a bijection. Its inverse is
@h' = f' *** g'@. Indeed:

* Left inverse:
     Let @p :: (a,c)@.
     => h' (h p)
  making explicit that @p@ is a pair
      = h' (h (p1,p2))
  by definition of h
      = h' (f p1, g p2)
  by definition of h'
      = (f' (f p1), g' (g p2))
  since f' is the inverse of f, and g' is the inverse of g
      = (p1, p2)
  since p = (p1,p2)
      = p

* Right inverse:
     Let @q :: (b,d)@.
     => h (h' q)
  making explicit that @q@ is a pair
      = h (h' (q1,q2))
  by definition of h'
      = h (f' q1, g' q2)
  by definition of h
      = (f (f' q1), g (g' q2))
  since f' is the inverse of f, and g' is the inverse of g
      = (q1, q2)
  since q = (q1,q2)
      = q

-}

---------------------------------
---------------------------------

instance Category Bijection where
  id = Bijection id id
  Bijection f f' . Bijection g g' = Bijection (f . g) (g' . f')

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

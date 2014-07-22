{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tach.DB.Types where

import           Control.Applicative
import           Data.Acid
import           Data.Bifunctor
import qualified Data.Foldable                         as F
import qualified Data.Sequence                         as SEQ
import           Data.Traversable
import           GHC.Generics
import           Tach.Class.Bounds
import           Tach.Class.Bounds
import qualified Tach.Class.Queryable                  as Q
import qualified Tach.DB.Types.Acid                    as Tach.DB.Types
import           Tach.DB.Types.Internal
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Impulse.Core
import           Tach.Transformable.Types.Wavelet
import           Tach.Transformable.Types.Wavelet.Core
import           Tach.Types.Classify
import           Tach.Types.Classify.Lens

newtype Transformed = Transformed { unTransformed ::  (Classify  ImpulseTransformed (Classify (WaveletTransformed Double) ())) } deriving (Show, Eq, Generic, Ord)


-- Just used for searching through a group of transformed.
emptyTransformed :: Transformed
emptyTransformed = Transformed . Unclassified $ Unclassified ()


type UncappedTransform a = Classify ImpulseTransformed (Classify (WaveletTransformed Double) a)

data TransformedInformation = TransformedInformation {
    transformedBounds :: (Int, Int)
  , transformedData   :: Transformed
} deriving (Show, Eq, Generic, Ord)

instance Bound Transformed where
    bounds tf = bounds . unTransformed $ tf

instance Q.Queryable Transformed TVNoKey where
    query step start end tf = Q.query step start end $ unTransformed tf


instance Bound TransformedInformation where
    bounds tf = transformedBounds tf

instance Q.Queryable TransformedInformation TVNoKey where
    query step start end tf = Q.query step start end $ transformedData tf

toTransformedInformation :: Transformed -> TransformedInformation
toTransformedInformation tf = TransformedInformation (bounds . unTransformed $ tf) tf

-- Do not just remove the type signature. It's there to keep track of migrations and to make sure the data is in the right format.
-- If you want to add something you HAVE TO MAKE SURE THE FIRST ITEMS ARE (Classify ImpulseTransformed (Classify (WaveletTransformedDouble) ....)) IN ORDER FOR THE
-- JSON SERIALIZATION TO WORK PROPERLY
transformAll :: [TVNoKey] -> SEQ.Seq Transformed
transformAll raw = endTransform <$> transformToWavelet [(Classified (transformImpulse raw))]
    where transformToWavelet = transformWavelet _implsLens _wvltLens toWavelet


toWavelet :: (Functor f) => f (Classify (WaveletTransformed Double) [TVNoKey]) -> f (Classify ImpulseTransformed (Classify (WaveletTransformed Double) c))
toWavelet = fmap towvfunc
  where towvfunc (Classified a) = Unclassified (Classified a)
        towvfunc (Unclassified g) = Classified $ transformImpulse g

endTransform :: UncappedTransform a -> Transformed
endTransform tf = Transformed $  (second . second) (\_ -> ()) tf


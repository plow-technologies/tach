module Tach.DB.Types where

import           Control.Applicative
import           Data.Acid
import           Data.Bifunctor
import qualified Data.Foldable                         as F
import qualified Data.Sequence                         as SEQ
import           Data.Traversable
import           Tach.DB.Types.Internal
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Impulse.Core
import           Tach.Transformable.Types.Wavelet
import           Tach.Transformable.Types.Wavelet.Core
import           Tach.Types.Classify
import           Tach.Types.Classify.Lens
import qualified Tach.DB.Types.Acid as Tach.DB.Types


newtype TransformedStore = TransformedStore { unTransformed ::  (Classify  ImpulseTransformed (Classify (WaveletTransformed Double) ())) }

type UncappedTransform a = Classify ImpulseTransformed (Classify (WaveletTransformed Double) a)

-- Do not just remove the type signature. It's there to keep track of migrations and to make sure the data is in the right format.
-- If you want to add something you HAVE TO MAKE SURE THE FIRST ITEMS ARE (Classify ImpulseTransformed (Classify (WaveletTransformedDouble) ....)) IN ORDER FOR THE
-- JSON SERIALIZATION TO WORK PROPERLY
transformAll :: [TVNoKey] -> SEQ.Seq TransformedStore
transformAll raw = endTransform <$> transformToWavelet [(Classified (transformImpulse raw))]
    where transformToWavelet = transformWavelet _implsLens _wvltLens toWavelet


toWavelet :: (Functor f) => f (Classify (WaveletTransformed Double) [TVNoKey]) -> f (Classify ImpulseTransformed (Classify (WaveletTransformed Double) c))
toWavelet = fmap towvfunc
  where towvfunc (Classified a) = Unclassified (Classified a)
        towvfunc (Unclassified g) = Classified $ transformImpulse g

endTransform :: UncappedTransform a -> TransformedStore
endTransform tf = TransformedStore $  (second . second) (\_ -> ()) tf
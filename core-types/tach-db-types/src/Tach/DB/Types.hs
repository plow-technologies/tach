module Tach.DB.Types where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.Foldable                    as F
import qualified Data.Sequence                    as SEQ
import           Data.Traversable
import           Tach.DB.Types.Internal
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Wavelet
import           Tach.Transformable.Types.Wavelet.Core
import           Tach.Types.Classify
import           Tach.Types.Classify.Lens


type Transformed = Classify  ImpulseTransformed (Classify (WaveletTransformed Double) ())

-- Do not just remove the type signature. It's there to keep track of migrations and to make sure the data is in the right format.
-- If you want to add something you HAVE TO MAKE SURE THE FIRST ITEMS ARE (Classify ImpulseTransformed (Classify (WaveletTransformedDouble) ....)) IN ORDER FOR THE
-- JSON SERIALIZATION TO WORK PROPERLY
transformAll :: [TVNoKey] -> SEQ.Seq (Classify  ImpulseTransformed (Classify (WaveletTransformed Double) c))
transformAll raw = transformToWavelet [(Classified (transformImpulse raw))]
    where transformToWavelet = transformWavelet _implsLens _wvltLens toWavelet


toWavelet :: (Functor f) => f (Classify (WaveletTransformed Double) [TVNoKey]) -> f (Classify ImpulseTransformed (Classify (WaveletTransformed Double) c))
toWavelet = fmap towvfunc
  where towvfunc (Classified a) = Unclassified (Classified a)
        towvfunc (Unclassified g) = Classified $ transformImpulse g

transformingThing :: (Classify a [Classify b c]) -> [Classify a (Classify b c)]
transformingThing (Unclassified cl) = map Unclassified cl
transformingThing (Classified un) = [Classified un]

otherTransformingThing :: Classify a b -> (Classify c (Classify a b))
otherTransformingThing = Unclassified

flipClassify :: Classify a (Classify b c) -> Classify b (Classify a c)
flipClassify (Classified a) = Unclassified (Classified a)
flipClassify (Unclassified (Classified a)) = Classified a
flipClassify (Unclassified (Unclassified b)) = Unclassified (Unclassified b)

stopper :: (a -> [Classify b c]) -> (t -> [Classify a1 a]) -> t -> [Classify b (Classify a1 c)]
stopper f g list =
    let firstList = g list
        secondList = map (fmap f) firstList
    in concat $ map ((map flipClassify) . transformingThing) secondList

glue :: (a -> [Classify b c]) -> (d -> [Classify e a]) -> [Classify f d] -> [Classify b (Classify e (Classify f c))]
glue f g list =
    let firstList = concat $ (map ((map flipClassify) . transformingThing)) $ (second  g) <$> list
        fList = map ((second transformingThing) . ((second . second) f)) firstList
    in concat $ map (( map (flipClassify . (second flipClassify)) . transformingThing)) fList
--reverseTransforms :: Classify (Classify a b) c ->

--betterGlue :: (Unclassifiable a) => (a -> Classify b a) -> (a -> Classify c a) -> [a] -> [(Classify c (Classify b a))]
--betterGlue f g raw =
--    let mUnclassified = unclassify

--attemptDeclassification :: (a -> [Classify b c]) ->

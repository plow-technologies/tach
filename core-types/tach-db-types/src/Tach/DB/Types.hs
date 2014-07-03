module Tach.DB.Types where

import Tach.DB.Types.Internal
import Tach.Transformable.Types.Wavelet
import Tach.Transformable.Types.Impulse
import Data.Bifunctor
import Control.Applicative
import Data.Traversable
import Tach.Types.Classify
import Tach.Types.Classify.Lens
import qualified Data.Foldable as F
import           Tach.Impulse.Types.TimeValue



transformAll raw = transformToWavelet2 . F.toList $ transformToWavelet [(Classified (transformImpulse raw))]
    where transformToWavelet = transformWavelet _implsLens _wvltLens toWavelet
          transformToWavelet2 = transformWavelet _implsLens _wvltLens2 toWavelet


toWavelet :: (Functor f) => f (Classify (WaveletTransformed Double) [TVNoKey]) -> f (Classify ImpulseTransformed (Classify (WaveletTransformed Double) c))
toWavelet = fmap towvfunc
  where towvfunc (Classified a) = Unclassified (Classified a)
        towvfunc (Unclassified g) = Classified $ transformImpulse g
--[Cl APeriodic (Cl WVLT (CL Const ())))] -> [Cl APeriodic (Cl WVLT (CL Const ())))]

----c1 = Cl a (Cl b [Cl c [Cl d e]])
----transformingThingtransformintransformingThinggThing
----t1 = over _unclassified transformThing c1 -> c2 = Cl a [Cl b (Cl c [Cl d e])] 
----t2 = transformThing c2 -> c3 = [Cl a (Cl b (Cl c [Cl d e]))]
----t3 = over traverse._unclassified transformThing -> 

----t1' = let 
----         es = catPrisms _unclassified._unclassified.traverse._unclassified c1
----         ds = catPrisms _unclassified._unclassified.traverse._classified c1
----         cs = catPrisms _unclassified._classified c1
----         bs = catPrisms _classified c1
--         --as = catPrisms _classified c1

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
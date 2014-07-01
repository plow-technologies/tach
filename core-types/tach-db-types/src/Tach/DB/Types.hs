module Tach.DB.Types where

import Tach.DB.Types.Internal
import Tach.Transformable.Types.Wavelet
import Tach.Transformable.Types.Impulse
import Data.Bifunctor
import Control.Applicative
import Data.Traversable
import Tach.Types.Classify

transformAll raw = stopper (transformImpulse) (transformWavelet) raw

transformingThing :: (Classify a [Classify b c]) -> [Classify a (Classify b c)]
transformingThing (Unclassified cl) = map otherTransformingThing cl
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
--glue :: (b1 -> [Classify b2 c]) -> (b -> [Classify a b1]) -> [Classify b3 b] -> [Classify a (Classify b3 (Classify b2 c))]
glue f g list = 
    let firstList = concat $ (map ((map flipClassify) . transformingThing)) $ (second  g) <$> list
        tList = ((second . second) f) <$> firstList
        fList = map (second transformingThing) tList
        glist = concat $ map (transformingThing) fList
        hlist = map ((second ) flipClassify) glist
        ilist = map flipClassify hlist
        --secondList = concat $ (map ((map flipClassify) . transformingThing)) $ (\x -> (second . second) f x) <$> firstList
        --secondList = (map (map flipClassify)) $ (second . second $ f) <$> firstList
    in ilist
--reverseTransforms :: Classify (Classify a b) c -> 
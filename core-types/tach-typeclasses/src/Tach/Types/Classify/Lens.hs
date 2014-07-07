{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Tach.Types.Classify.Lens where

import           Control.Lens
import           Tach.Types.Classify





--_wvltLens :: (Choice p, Applicative f) => p a1 (f a2) -> p (Classify a0 (Classify a1 b0)) (f (Classify a0 (Classify a2 b0)))
_wvltLens :: Prism (Classify c (Classify a d)) (Classify c (Classify b d)) a b
_wvltLens = _Unclassified . _Classified

_wvltLens2 :: Prism (Classify c (Classify d (Classify a e))) (Classify c (Classify d (Classify b e))) a b
_wvltLens2 = _Unclassified . _Unclassified . _Classified

_implsLens :: Prism (Classify a c) (Classify b c) a b
_implsLens = _Classified

_othrsLens :: Prism (Classify c a) (Classify c b) a b
_othrsLens = _Unclassified
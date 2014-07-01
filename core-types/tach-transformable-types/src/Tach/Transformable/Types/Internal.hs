module Tach.Transformable.Types.Internal
    (
        eitherToClassify
    ) where

import           Data.Either
import           Tach.Types.Classify

eitherToClassify :: Either b a -> Classify a b
eitherToClassify (Left x) = Unclassified x
eitherToClassify (Right y) = Classified y

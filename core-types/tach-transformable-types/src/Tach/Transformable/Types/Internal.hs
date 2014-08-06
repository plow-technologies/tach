
module Tach.Transformable.Types.Internal
   (  eitherToClassify
      ) where

import Tach.Types.Classify

-- Shouldn't we just move this to Tach.Types.Classify?

eitherToClassify :: Either b a -> Classify a b
eitherToClassify (Left x) = Unclassified x
eitherToClassify (Right y) = Classified y

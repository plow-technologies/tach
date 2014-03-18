{-# LANGUAGE OverloadedStrings #-}


module Tach.Impulse.Types.Class.Retrievable where 


-- | Retrievable is the typeclass that unifies the language of 
-- retrieval over different series.  
class Retrievable series where
  select :: query -> targetSeries -> result  

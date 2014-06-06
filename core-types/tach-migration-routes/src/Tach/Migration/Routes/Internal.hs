module Tach.Migration.Routes.Internal
    (
      groupUp
    ) where
groupUp :: Int -> [t] -> [[t]]
groupUp n lst = loop [] lst
 where 
   loop l [] = l 
   loop l ol = let (t,r) = splitAt n ol
               in loop (t:l) $! r
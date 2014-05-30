module Main where

import Tach.Periodic
import qualified Data.Vector.Storable as VS

main :: IO ()
main = do
  let ys = [0,10..200] ++ [230,245..845] ++ [910,940..1210] ++ [1250,1265..2315000] :: [Int]
  return ()

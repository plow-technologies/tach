module Main where


import Tach.Periodic

main :: IO ()
main = do
  let ys = [0,10..200] ++ [230,245..845] ++ [910,940..1210] ++ [1250,1265..23150000] :: [Int]
      xs = classifyData 15 1 0 id ys
  putStrLn . show $ xs
  return ()

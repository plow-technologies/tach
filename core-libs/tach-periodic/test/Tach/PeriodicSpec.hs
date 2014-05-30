module Tach.PeriodicSpec (main, spec) where

import Test.Hspec
import Tach.Periodic
import qualified Data.Sequence as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Unclassified data should be the same as classified data" $ do
    it "should work fine" $ do
      let ys = [0,10..200] ++ [230,245..845] ++ [910,940..1210] ++ [1250,1265..2315] :: [Int]
      (classifyData 15 1 0 id ys ) `shouldBe` [TVAPeriodic (APeriodicData {unAPeriodicData = S.fromList [0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200]})
                                                            ,TVPeriodic (PeriodicData {unPeriodicData = S.fromList [230,245,260,275,290,305,320,335,350,365,380,395,410,425,440,455,470,485,500,515,530,545,560,575,590,605,620,635,650,665,680,695,710,725,740,755,770,785,800,815,830,845]})
                                                            ,TVAPeriodic (APeriodicData {unAPeriodicData = S.fromList [910,940,970,1000,1030,1060,1090,1120,1150,1180,1210]})
                                                            ,TVPeriodic (PeriodicData {unPeriodicData = S.fromList [1250,1265,1280,1295,1310,1325,1340,1355,1370,1385,1400,1415,1430,1445,1460,1475,1490,1505,1520,1535,1550,1565,1580,1595,1610,1625,1640,1655,1670,1685,1700,1715,1730,1745,1760,1775,1790,1805,1820,1835,1850,1865,1880,1895,1910,1925,1940,1955,1970,1985,2000,2015,2030,2045,2060,2075,2090,2105,2120,2135,2150,2165,2180,2195,2210,2225,2240,2255,2270,2285,2300,2315]})]


-- Possibly the best test for this is to generate data that is preclassified, 
-- unclassify it, and then test the classified function against the preclassified function

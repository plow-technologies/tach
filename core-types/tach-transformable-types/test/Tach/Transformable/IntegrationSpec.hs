module Tach.Transformable.IntegrationSpec (main, spec) where

import           Control.Applicative
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import           Numeric.Integration.TanhSinh
import           Numeric.Tools.Interpolation
import           System.IO
import           System.Random
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse
import           Test.Hspec
import Test.HUnit.Base


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integrating simple TVNoKey lists" $ do
    it "Take the integral of three points" $ do
      let tvNoKeyList = [TVNoKey 0 2, TVNoKey 1 5, TVNoKey 5 2]
          mesh = createLinearInterp tvNoKeyList
      integrateWindow mesh (V.fromList (tvNkSimpleTime <$> tvNoKeyList)) (0,5) `shouldBe` 17.5
    it "Take the integral of three points and get the weighted average" $ do
      let tvNoKeyList = [TVNoKey 0 2, TVNoKey 1 5, TVNoKey 5 2]
          mesh = createLinearInterp tvNoKeyList
          times = (V.fromList (tvNkSimpleTime <$> tvNoKeyList))
      weightedAverageWindow mesh times (0,5) `shouldBe` (17.5/5)
    it "Should take the integral of uniform points and be within 1% of the actual average (error is due to floating point arithmetic" $ do
      gen <- getStdGen
      let vals = randomList 100 (500,900) gen
          tvNoKeyList = zipWith (TVNoKey) [1,2..] vals
          mesh = createLinearInterp tvNoKeyList
          times = (V.fromList (tvNkSimpleTime <$> tvNoKeyList))
          average = ((sum vals) / 100)
      shouldBeWithin 0.01 (weightedAverageWindow mesh times (1,100)) average
    it "Should take the integral and match the integration library within 10% (floating point errors and approx. integration)" $ do
      gen <- getStdGen
      let vals = randomList 20 (600,1000) gen
          tvNoKeyList = zipWith TVNoKey [1,2..] vals
          mesh = createLinearInterp tvNoKeyList
          times = (V.fromList (tvNkSimpleTime <$> tvNoKeyList))
          aprox = result . (\xs -> xs !! 0) $ simpson (at mesh) (fromIntegral $ V.head times) (fromIntegral $ V.last times)
          res = integrateWindow mesh times (1,20)
      putStrLn $  show aprox ++ " - " ++ show res
      shouldBeWithin 0.1 res aprox
    it "Should integrate a constant across a non-uniform time list and remain at the constant for the average" $ do
      gen <- getStdGen
      let ts = randomList 20 (0,1000) gen :: [Int]
          tvNoKeyList = zipWith TVNoKey (S.toList . S.fromList $ ts) [20,20..]
          mesh = createLinearInterp tvNoKeyList
          times = (V.fromList (tvNkSimpleTime <$> tvNoKeyList))
          res = weightedAverageWindow mesh times (V.head times, V.last times)
      putStrLn $ "actual: " ++ (show 20) ++ " result: " ++ show res
      shouldBeWithin 0.005 res 20

shouldBeWithin :: (Ord a, Num a, Show a) => a -> a -> a -> IO ()
shouldBeWithin percent result expected = do
  let res = withinPercent percent expected result
  if res
    then return ()
    else assertFailure $ (show result) ++ " is not within " ++ (show percent) ++ "% of the expected: " ++ (show expected)

withinPercent ::  (Ord a, Num a) => a -> a -> a -> Bool
withinPercent percent expected result = above && below
  where above = result > expected - delta
        below = result < expected + delta
        delta = expected * percent

randomList :: (RandomGen g, Random a) => Int -> (a,a) -> g -> [a]
randomList 0 _ _ = []
randomList n bounds gen = a:(randomList (n-1) bounds gen')
  where (a, gen') = randomR bounds gen

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Tach.Periodic where

import Tach.Periodic.Internal
import Data.Either
import Data.List
import qualified Data.Vector.Storable as VS
import Safe
import System.Random
import Control.Applicative
import GHC.Generics

newtype PeriodicData a = PeriodicData  { unPeriodicData :: VS.Vector a } deriving (Eq, Show, Generic)
newtype APeriodicData a = APeriodicData { unAPeriodicData :: VS.Vector a } deriving (Eq, Show, Generic)
data TVData a = TVPeriodic (PeriodicData a) | TVAPeriodic (APeriodicData a) deriving (Eq, Show, Generic)

data PeriodicFolding a = PeriodicFolding {
  firstPeriodic :: Int
 ,periodicCount :: Int
 ,timeValueData :: [TVData a]
}

tvDataToEither :: TVData a -> Either (APeriodicData a) (PeriodicData a)
tvDataToEither (TVPeriodic x) = Right x
tvDataToEither (TVAPeriodic x) = Left x

--testData :: IO [TVData Double]
--testData = do
--  rData <- genRandomData (0,10000) (10,100)
--  return $ combineAperiodic $ classifyPeriodic 10 10 rData

testList :: [Double]
testList = aperiodicTimeData 515.0 1000.0 100

combineAperiodic :: (VS.Storable a) => [TVData a] -> [TVData a]
combineAperiodic = foldl' combineAperiodicFold []

classifyData :: (Num a, VS.Storable b, Ord a) => a -> a -> Int -> (b -> a) -> [b] -> [TVData b]
classifyData  period delta minPeriodicSize toNumFunc list = combineAperiodic . (removePeriodicBelow minPeriodicSize) $ classifyPeriodic period delta toNumFunc list

removePeriodicBelow :: (VS.Storable a) => Int -> [TVData a] -> [TVData a]
removePeriodicBelow minSize list = map (setAperiodicBelow minSize)  list

setAperiodicBelow :: (VS.Storable a) => Int -> (TVData a) -> (TVData a)
setAperiodicBelow minSize val@(TVPeriodic (PeriodicData periodic)) =
  if ((VS.length periodic) < minSize)
    then TVAPeriodic $ APeriodicData periodic
    else val
setAperiodicBelow _ b = b

-- Appends the TVData to the last element of the TVData list if both are aperiodic
combineAperiodicFold :: (VS.Storable a) => [TVData a] -> TVData a -> [TVData a]
combineAperiodicFold list item = 
  let first = lastMay list
  in case first of
    Nothing -> [item]
    (Just (TVAPeriodic (APeriodicData periodicList))) ->
      case item of
        (TVAPeriodic (APeriodicData a)) ->
          (init list) ++ [(TVAPeriodic $ APeriodicData (periodicList VS.++ a))]
        _ -> list ++ [item]
    (Just (TVPeriodic (PeriodicData _))) -> list  ++ [item]

classifyPeriodic :: (Num a, Ord a, VS.Storable b) => a -> a -> (b -> a) -> [b] -> [TVData b]
classifyPeriodic period delta toNumFunc list = foldl' (takePeriodic period delta toNumFunc) [] list

-- | The function that folds over a list and looks for any matches in a period
takePeriodic :: (Num a, Ord a, VS.Storable b) => a -> a -> (b -> a) -> [TVData b] -> b -> [TVData b]
takePeriodic period delta toNumFunc old current = 
  let maxPeriod = period + delta
      minPeriod = period - delta
      mLast = lastMay old
  in case mLast of
    Nothing ->
      [TVAPeriodic $ APeriodicData (VS.singleton current)]
    (Just (TVPeriodic (PeriodicData periodData))) ->
      let firstVal = VS.head periodData
          size = VS.length periodData
          difference = abs $ (toNumFunc current) - (toNumFunc firstVal)
      in if ((difference <= maxPeriod) && (difference >= minPeriod))
        then ((init old) ++ [(TVPeriodic . PeriodicData $ VS.snoc  periodData current)])
        else ((old) ++ [(TVAPeriodic $ APeriodicData (VS.singleton current))])
    (Just (TVAPeriodic (APeriodicData aperiodicData))) -> 
      let lastVal = VS.last aperiodicData
          difference = abs $ (toNumFunc current) - (toNumFunc lastVal)
      in if ((difference <= maxPeriod) && (difference >= minPeriod))
        then ((init old) ++ [(TVPeriodic . PeriodicData $ VS.snoc  aperiodicData current)])
        else (old ++ [(TVAPeriodic $ APeriodicData (VS.singleton current))])


periodStart :: Double -> Int -> Int -> [Int]
periodStart start count period = take count [(round start) + (x*period) | x <- [0..]]


--classifiy :: [(Double, Double)] -> VS.Vector 
--classify xs = foldl' addPeriodic []

--addPeriodic :: 

linSpace :: Double -> Double -> Int -> [Double]
linSpace start end n
  | n-1 <= 0 = []
  | otherwise = 
      map (\x -> (x * mult) + start) (take (n - 1) [0..])
      where mult = (end - start) / (fromIntegral l)
            l = n-1 :: Int

aperiodicTimeData :: Double -> Double -> Int -> [Double]
aperiodicTimeData start end n = [x+(sin x) | x <- (linSpace start end n)]


randomChunks :: Double -> Double -> Double -> Double -> IO [(Double,Double)]
randomChunks start end minStep maxStep
  | (end - start) < minStep = return [(start,end+minStep)]
  | otherwise = do
    step <- randomRIO (minStep,maxStep)
    let newEnd = start+step
    case newEnd > end of
      True -> return [(start,end)]
      False -> do
        xs <- randomChunks (newEnd + 16) end minStep maxStep
        return $ (start,newEnd):xs

getRandomList :: Int -> (Int,Int) -> IO [Int]
getRandomList size range = mapM (\_ -> randomRIO range) [1..size]

randomData :: (Double,Double) -> IO (TVData Double)
randomData (start,end) = do
  randomChoice <- randomRIO (0,10) :: IO Int
  case randomChoice of
    _
      | randomChoice <= 5 -> do --Get a period with 15 second intervals
          let xs = linSpace start end (round ((end - start)/15))
          return . TVPeriodic . PeriodicData . VS.fromList $ xs
      | otherwise -> do
        randomCount <- randomRIO ((end - start)/15, (end - start)/200)
        let xs = aperiodicTimeData start end (round randomCount)
        return . TVAPeriodic . APeriodicData . VS.fromList $ xs

genRandomData :: (Double,Double) -> (Double, Double) -> IO [(TVData Double)]
genRandomData (start,end) (minStep,maxStep) = do
  chunks <- randomChunks start end minStep maxStep
  chunkedData <- mapM randomData chunks
  return . removeEmptyRandoms $ chunkedData

removeTVData :: (Num a, Ord a, VS.Storable a) => [TVData a] -> [a]
removeTVData list = foldl' removeTVDataFold [] list

removeTVDataFold :: (Num a, Ord a, VS.Storable a) => [a] -> TVData a -> [a]
removeTVDataFold list item =list ++ (VS.toList . unTVData $ item)


unTVData :: (Num a, Ord a, VS.Storable a) => TVData a -> VS.Vector a
unTVData (TVPeriodic (PeriodicData c)) = c
unTVData (TVAPeriodic (APeriodicData b)) = b

removeEmptyRandoms :: [TVData Double] -> [TVData Double]
removeEmptyRandoms xs = foldl' remEmpty [] xs


remEmpty :: [TVData Double] -> TVData Double -> [TVData Double]
remEmpty ys xs@(TVPeriodic (PeriodicData x)) =
  if (VS.null x) then ys else (ys ++ [xs])
remEmpty ys xs@(TVAPeriodic (APeriodicData x)) =
  if (VS.null x) then ys else (ys ++ [xs])
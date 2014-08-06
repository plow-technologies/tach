
{-# LANGUAGE DeriveGeneric #-}

module Tach.Periodic where

import Prelude hiding (foldl)
--import Tach.Periodic.Internal
import System.Random
import Control.Applicative
import GHC.Generics
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad (replicateM)

newtype PeriodicData  a = PeriodicData  { unPeriodicData  :: S.Seq a } deriving (Eq, Show, Generic)

newtype APeriodicData a = APeriodicData { unAPeriodicData :: S.Seq a } deriving (Eq, Show, Generic)

data TVData a = TVPeriodic (PeriodicData a) | TVAPeriodic (APeriodicData a) deriving (Eq, Show, Generic)

data PeriodicFolding a = PeriodicFolding {
   firstPeriodic :: Int
 , periodicCount :: Int
 , timeValueData :: [TVData a]
   }

tvDataToEither :: TVData a -> Either (S.Seq a) (PeriodicData a)
tvDataToEither (TVPeriodic x) = Right x
tvDataToEither (TVAPeriodic x) = Left $ unAPeriodicData x

--testData :: IO [TVData Double]
--testData = do
--  rData <- genRandomData (0,10000) (10,100)
--  return $ combineAperiodic $ classifyPeriodic 10 10 rData

testList :: [Double]
testList = aperiodicTimeData 515.0 1000.0 100

combineAperiodic :: S.Seq (TVData a) -> S.Seq (TVData a)
combineAperiodic = F.foldl combineAperiodicFold S.empty

-- | Appends the TVData to the last element of the TVData list if both are aperiodic.
combineAperiodicFold :: S.Seq (TVData a) -> TVData a -> S.Seq (TVData a)
combineAperiodicFold list item =
  case (S.viewr list,item) of
    (   listInit S.:> TVAPeriodic (APeriodicData periodicList)
      , TVAPeriodic (APeriodicData aSeq)
        ) -> listInit S.|> TVAPeriodic (APeriodicData $ periodicList S.>< aSeq)
    _ -> list S.|> item        

classifyData :: (Num a, Ord a, F.Foldable f) => a -> a -> Int -> (b -> a) -> f b -> S.Seq (TVData b)
classifyData period delta minPeriodicSize toNumFunc =
    combineAperiodic
  . removePeriodicBelow minPeriodicSize
  . classifyPeriodic period delta toNumFunc
  . fromFoldable

removePeriodicBelow :: Int -> S.Seq (TVData a) -> S.Seq (TVData a)
removePeriodicBelow = (<$>) . setAperiodicBelow

setAperiodicBelow :: Int -> TVData a -> TVData a
setAperiodicBelow minSize val@(TVPeriodic (PeriodicData periodic)) =
  if S.length periodic < minSize
     then TVAPeriodic $ APeriodicData periodic
     else val
setAperiodicBelow _ b = b

classifyPeriodic :: (Num a, Ord a) => a -> a -> (b -> a) -> S.Seq b -> S.Seq (TVData b)
classifyPeriodic period delta toNumFunc = F.foldl (takePeriodic period delta toNumFunc) S.empty

fromFoldable :: F.Foldable f => f a -> S.Seq a
fromFoldable = F.foldl (S.|>) S.empty 

-- | The function that folds over a list and looks for any matches in a period
takePeriodic :: (Num a, Ord a) => a -> a -> (b -> a) -> S.Seq (TVData b) -> b ->  S.Seq (TVData b)
takePeriodic period delta toNumFunc old current = 
  let maxPeriod = period + delta
      minPeriod = period - delta
      mLast = lastMaySeq old
  in case mLast of
    Nothing ->
      S.singleton $ TVAPeriodic $ APeriodicData (S.singleton current)
    (Just (TVPeriodic (PeriodicData periodData))) ->
      let firstVal = lastSeq periodData
          difference = abs $ (toNumFunc current) - (toNumFunc firstVal)
      in if ((difference <= maxPeriod) && (difference >= minPeriod))
        then ((initSeq old) S.|> (TVPeriodic . PeriodicData $  periodData S.|> current))
        else ((old) S.|> (TVAPeriodic $ APeriodicData (S.singleton current)))
    (Just (TVAPeriodic (APeriodicData aperiodicData))) -> 
      let lastVal = lastSeq aperiodicData
          difference = abs $ (toNumFunc current) - (toNumFunc lastVal)
      in if ((difference <= maxPeriod) && (difference >= minPeriod))
        then ((initSeq old) S.|> (TVPeriodic . PeriodicData $ aperiodicData S.|> current))
        else (old S.|> (TVAPeriodic $ APeriodicData (S.singleton current)))

-----------------------------
-- Sequence related functions
--
-- Note: This functions should probably be moved to another 'utility' package.
--

-- Unsafe! :(
headSeq :: S.Seq a -> a
headSeq = fromJust . headMaySeq

headMaySeq :: S.Seq a -> Maybe a
headMaySeq aSeq =
  case S.viewl aSeq of
    x S.:< _ -> Just x
    _ -> Nothing

initSeq :: S.Seq a -> S.Seq a
initSeq aSeq = S.take (S.length aSeq - 1) aSeq

-- Unsafe! :(
lastSeq :: S.Seq a -> a
lastSeq = fromJust . lastMaySeq

lastMaySeq :: S.Seq a -> Maybe a
lastMaySeq aSeq = 
  case S.viewr aSeq of
    _ S.:> x -> Just x
    _ -> Nothing

-----------------------------

periodStart :: Double -> Int -> Int -> [Int]
periodStart start count period = take count [round start + x*period | x <- [0..]]

--classifiy :: [(Double, Double)] -> V.Vector 
--classify xs = foldl' addPeriodic []

--addPeriodic :: 

-- | Calling @linSpace start end n@ returns @fmap f [0..n-1]@, where @f x = x*k + start@,
--   and @k = (end - start) / fromIntegral l@. Note that @f 0 = start@, @f (n-1) = end@,
--   and @length (linSpace start end n) = n@.
--
linSpace :: Double -- ^ First element of the list.
         -> Double -- ^ Last element (approximately) of the list.
         -> Int    -- ^ Lenght of the list.
         -> [Double] 
linSpace start end n = take n $ iterate (+k) start
  where
    k = (end - start) / fromIntegral (n-1)

aperiodicTimeData :: Double -> Double -> Int -> [Double]
aperiodicTimeData start end n = [ x + sin x | x <- linSpace start end n ]

randomChunks :: Double -> Double -> Double -> Double -> IO [(Double,Double)]
randomChunks start end minStep maxStep
    -- Why this instead of [], or [(start,end)]. What's the rationale behind
    -- this design choice? I could understand it if we were giving importance
    -- to keep the property that size of each chunk is at least 'minStep',
    -- but this property is broken later. (Daniel)
  | end - start < minStep = return [(start,end+minStep)]
  | otherwise = do
    step <- randomRIO (minStep,maxStep)
    let newEnd = start+step
    case newEnd > end of
      True -> return [(start,end)]
      False -> do
        -- Why this (+16) ?? (Daniel)
        xs <- randomChunks (newEnd + 16) end minStep maxStep
        return $ (start,newEnd):xs

getRandomList :: Int -> (Int,Int) -> IO [Int]
getRandomList size = replicateM size . randomRIO

randomData :: (Double,Double) -> IO (TVData Double)
randomData (start,end) = do
  randomChoice <- randomRIO (0,10) :: IO Int
  case randomChoice of -- Maybe using 'if' statement will be cleaner here.
    _
      | randomChoice <= 5 -> do --Get a period with 15 second intervals
          let xs = linSpace start end (round ((end - start)/15))
          return . TVPeriodic . PeriodicData . S.fromList $ xs
      | otherwise -> do
        randomCount <- randomRIO ((end - start)/15, (end - start)/200)
        let xs = aperiodicTimeData start end (round randomCount)
        return . TVAPeriodic . APeriodicData . S.fromList $ xs

genRandomData :: (Double,Double) -> (Double, Double) -> IO [(TVData Double)]
genRandomData (start,end) (minStep,maxStep) = do
  chunks <- randomChunks start end minStep maxStep
  chunkedData <- mapM randomData chunks
  return . removeEmptyRandoms $ chunkedData

removeTVData :: (Num a, Ord a) => [TVData a] -> [a]
removeTVData = F.foldl' removeTVDataFold []

removeTVDataFold :: (Num a, Ord a) => [a] -> TVData a -> [a]
removeTVDataFold list item = list ++ (F.toList . unTVData $ item)

unTVData :: (Num a, Ord a) => TVData a -> S.Seq a
unTVData (TVPeriodic (PeriodicData c)) = c
unTVData (TVAPeriodic (APeriodicData b)) = b

removeEmptyRandoms :: [TVData Double] -> [TVData Double]
removeEmptyRandoms = F.foldl' remEmpty []

remEmpty :: [TVData Double] -> TVData Double -> [TVData Double]
remEmpty ys xs@(TVPeriodic (PeriodicData x)) =
  if S.null x then ys else ys ++ [xs]
remEmpty ys xs@(TVAPeriodic (APeriodicData x)) =
  if S.null x then ys else ys ++ [xs]

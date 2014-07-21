{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Tach.Transformable.Types.Impulse.Core where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.Foldable                     as F
import qualified Data.Sequence                     as S
import           Data.Traversable
import           Data.Typeable
import qualified Data.Vector                       as V
import qualified Data.Vector.Generic               as GV
import qualified Data.Vector.Unboxed               as U
import           Data.Wavelets.Construction
import           Data.Wavelets.Reconstruction
import           Data.Wavelets.Scaling
import           GHC.Generics
import           Numeric.Classes.Indexing
import           Numeric.Tools.Integration
import           Numeric.Tools.Interpolation
import           Plow.Extras.Lens
import           Statistics.Function
import           Tach.Class.Bounds
import qualified Tach.Class.Insertable             as I
import           Tach.Class.Queryable
import           Tach.Impulse.Types.TimeValue
import           Tach.Periodic
import           Tach.Periodic
import           Tach.Transformable.Types.Internal
import           Tach.Types.Classify



data ImpulseTransformed = ImpulseTransformed {
    impulseRepresentation :: LinearInterp (ImpulseMesh Double)
  , impulseStart          :: Int
  , impulseEnd            :: Int
} deriving (Show, Ord, Eq, Typeable)


impulseBounds :: ImpulseTransformed -> (Int, Int)
impulseBounds impls = (impulseStart impls, impulseEnd impls)

instance Bound (ImpulseTransformed) where
  bounds = impulseBounds

instance Queryable (ImpulseTransformed) TVNoKey where
  query step start end impls = I.toInsertable $ queryImpulse impls step start end

-- | Create a linear interpolation from a list of TVNoKeys in order to find values between points
createLinearInterp :: [TVNoKey] -> LinearInterp (ImpulseMesh Double)
createLinearInterp tvnklist = linearInterp $ tabulate mesh values
  where mesh = createMesh tvnklist
        values = V.fromList $ tvNkSimpleValue <$> tvnklist

-- | Create a mesh for linear interpolation
createMesh :: [TVNoKey] -> ImpulseMesh Double
createMesh tf = ImpulseMesh rep mn mx
  where rep = V.fromList $ fromIntegral . tvNkSimpleTime <$>  tf
        (mn,mx) = minMax rep


-- | Get all values
reconstructImpulse :: ImpulseTransformed -> V.Vector TVNoKey
reconstructImpulse tf = GV.zipWith (\t v -> TVNoKey (round t) v) times vals
  where interp = impulseRepresentation tf
        times = impulseMeshRep . interpolationMesh $ interp
        vals = GV.fromList . GV.toList . interpolationTable $ interp


queryImpulseSmooth :: ImpulseTransformed -> Int -> Int -> Int -> [TVNoKey]
queryImpulseSmooth = queryLinearIterpSmooth . impulseRepresentation


-- | Query a Linear Interpolation store given a step and bounds. The result will be a list of TVNoKeys
-- with a length of ((end - start) / step) and a value of the average value surrounding the key from t + (step/2) and t - (step/2)
queryLinearIterpSmooth :: LinearInterp (ImpulseMesh Double) -> Int -> Int -> Int -> [TVNoKey]
queryLinearIterpSmooth interp step start end = (\(time, bnds) -> TVNoKey time (weightedAverageWindow interp bnds)) <$> windows
    where halfStep = step `div` 2
          times = [start,start+step..end]
          windows = (\t -> (t,(trimWindowToBounds (start,end)) . createBounds $ t)) <$> times
          createBounds x = (x - halfStep, x + halfStep)

-- | Integrate over an entire window and then divide by the window size in order to find the
-- average of the area
weightedAverageWindow :: LinearInterp (ImpulseMesh Double) -> (Int, Int) -> Double
weightedAverageWindow interp window@(start,end) = (integrateWindow interp window) / dt
  where dt = fromIntegral $ end - start


-- | Find the integral for a series of points given a list of times to shorten the number of
-- calculations to be a faster O(n)
--
integrateWindow :: LinearInterp (ImpulseMesh Double) -> (Int, Int) -> Double
integrateWindow interp (start,end) = sumRes
  where times = V.map round $ impulseMeshRep . interpolationMesh $ interp  -- Get a list of all times from the linear interpolator
        filteredTimes = V.findIndices (\t -> t > start && t < end) times   -- Filter all times that aren't between the bounds
        times' = V.cons start (V.snoc (V.map (times !) filteredTimes) end) -- Add the start and end times to the filtered times
        higherTimes = V.tail times'
        finalTimes = V.zip times' higherTimes                              -- Offset the times to make the bounds so [1,2,3,4] becomes [(1,2),(2,3),(3,4)]
        sumRes = V.sum $ V.map (calcArea interp) finalTimes                -- Add the results of the areas of each of the smaller times


-- | Takes the total bounds and the window bounds and then returns a modified window bounds such that
-- anything below the total bounds start is moved up to the start and anything after the total bounds end is moved down to the total bounds end
trimWindowToBounds :: (Int,Int) -> (Int, Int) -> (Int, Int)
trimWindowToBounds (totalStart, totalEnd) (windowStart, windowEnd) = (max totalStart windowStart, min totalEnd windowEnd)


-- | Calculate the area of a linear interpreted graph given a start and end
-- Used for the start and end of a window or the internal portion of a window
calcArea :: LinearInterp (ImpulseMesh Double) -> (Int, Int) -> Double
calcArea interp (x1,x2) = ((val1 + val2) / 2) * dt
 where dt = fromIntegral $ x2 - x1
       val1 = (at interp $ fromIntegral x1)
       val2 = (at interp $ fromIntegral x2)


-- | A mesh for the linear interpolation in order to keep track of times
-- of a mesh when compared to values
data ImpulseMesh a  = ImpulseMesh {
  impulseMeshRep   :: V.Vector a
, impulseMeshLower :: Double
, impulseMeshUpper :: Double
} deriving (Show, Ord, Eq, Typeable)

-- | Find the maximum index of an ImpulseMesh where x >= a where a is a value in the impulseMesh
findIndex :: ImpulseMesh Double -> Double -> Int
findIndex mesh x = V.maximum . V.findIndices (\a -> a <= x) $ impulseMeshRep mesh


instance (U.Unbox a) => Indexable (ImpulseMesh a) where
  type IndexVal (ImpulseMesh a) = a
  size = size . impulseMeshRep
  unsafeIndex = unsafeIndex . impulseMeshRep

instance Mesh (ImpulseMesh Double) where
  meshLowerBound = impulseMeshLower
  meshUpperBound = impulseMeshUpper
  meshFindIndex = findIndex


queryImpulse :: ImpulseTransformed -> Int -> Int -> Int -> V.Vector TVNoKey
queryImpulse tf step start end = trim . reconstructImpulse $ tf
  where trim = V.filter (\t -> tvNkSimpleTime t >= start && tvNkSimpleTime t <= end)

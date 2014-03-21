{-# LANGUAGE OverloadedStrings #-}

module Tach.Acid.Impulse.StateSpec (main, spec) where
-- Test Stuff
import Test.Hspec


-- Main Library Under Test 
import Tach.Acid.Impulse.Cruds


-- Other Libraries 
import Filesystem.Path
import Tach.Acid.Impulse.Cruds.Create
import Tach.Acid.Impulse.Cruds.Read
import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.TimeValueSeries
import Tach.Impulse.Types.Impulse
import Tach.Migration.Acidic.Types
import Data.Set
import Tach.Acid.Impulse.State

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False



buildTestImpulseRep :: [Integer] -> [Double] -> ImpulseRep (Set TVNoKey)
buildTestImpulseRep is ds = ImpulseRep . fromList $ zipWith bldFcn is ds 
    where 
      bldFcn i d = TVNoKey i d



buildTestImpulseKey :: Integer -> TVKey
buildTestImpulseKey i = ImpulseKey i 

buildTestImpulseStart :: Integer -> TVSStart
buildTestImpulseStart i = ImpulseStart i 

buildTestImpulseEnd :: Integer -> TVSEnd
buildTestImpulseEnd i = ImpulseEnd i 


-- | This field is left blank because no period information has been calculated yet so it should stay blank
buildTestImpulsePeriod :: TVPeriod
buildTestImpulsePeriod = initialImpulsePeriod



buildTestImpulseSeries :: Integer -> Integer -> Integer -> [Integer] -> [Double] 
                       -> ImpulseSeries TVKey TVPeriod TVSStart TVSEnd (ImpulseRep (Set TVNoKey))
buildTestImpulseSeries key start end is ds = ImpulseSeries                                 
                                             (buildTestImpulseKey key )                    
                                             (buildTestImpulsePeriod)
                                             (buildTestImpulseStart start)
                                             (buildTestImpulseEnd end )
                                             (buildTestImpulseRep is ds)

buildTestImpulseTypeStore ::Integer -> Integer -> Integer -> [Integer] -> [Double] 
                          -> TVSimpleImpulseTypeStore 
buildTestImpulseTypeStore key start end is ds = TVSimpleImpulseTypeStore 
                                                (buildTestImpulseSeries key start end is ds)


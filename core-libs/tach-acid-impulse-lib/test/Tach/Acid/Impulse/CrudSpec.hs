{-# LANGUAGE OverloadedStrings #-}

module Tach.Acid.Impulse.CrudSpec (main, spec) where
-- Test Stuff
import Test.Hspec

import System.Directory

-- Main Library Under Test 
import Tach.Acid.Impulse.Cruds

import Data.Acid
import Data.Acid.Advanced


-- Other Libraries 
import Filesystem.Path
import Tach.Acid.Impulse.Cruds.Create
import Tach.Acid.Impulse.Cruds.Read
import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.TimeValueSeries
import Tach.Impulse.Types.Impulse
import Tach.Migration.Acidic.Types
import Tach.Acid.Impulse.Cruds.Types
import Data.Set
import Tach.Acid.Impulse.State

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Insert" $ do
    it "Should insert and query the key" $ do
      let key = 1
          impulseStore = buildTestImpulseTypeStore key 0 200 [0..200] (take 200 [0.0,0.2..])
          nKey = TVNoKey 199 1001.3
      impulseState <- openLocalStateFrom "teststates/" (impulseStore)
      update' impulseState (InsertTVSimpleImpulse (buildTestImpulseKey key) nKey)
      eRes <- query' impulseState (GetTVSimpleImpulse (buildTestImpulseKey key) 200)
      closeAcidState impulseState
      removeDirectoryRecursive "teststates"
      case eRes of
        Left a -> expectationFailure $ "Error when accessing key: " ++ (show a)
        Right res -> res `shouldBe` nKey
  describe "InsertMany" $ do
    it "Should insert a set and query each item" $ do
      let key = 1
          impulseStore = buildTestImpulseTypeStore key 0 199 [1..199] (take 199 [0.0,2.0..])
          nKeys = fromList $ buildNoKeys [200..300] (take 100 $ drop 200 [0.0,2.0..])
          eSet = fromList [] :: Set TVNoKey
      impulseState <- openLocalStateFrom "teststates/" impulseStore
      update' impulseState (InsertManyTVSimpleImpulse (buildTestImpulseKey key) nKeys)
      eRes <- query' impulseState (GetTVSimpleImpulseMany (buildTestImpulseKey key) (ImpulseStart 200) (ImpulseEnd 300))
      closeAcidState impulseState
      removeDirectoryRecursive "teststates"
      case eRes of
        Left a -> expectationFailure $ "Error when accessing key: " ++ (show a)
        Right res -> res `shouldBe` nKeys
  describe "Delete" $ do
    it "Should delete and make sure it is deleted" $ do
      let key = 1
          impulseStore = buildTestImpulseTypeStore key 0 200 [0..200] (take 200 [0.2,0.4..])
          nKey = TVNoKey 0 0.0
      impulseState <- openLocalStateFrom "teststates/" (impulseStore)
      update' impulseState (DeleteTVSimpleImpulse (buildTestImpulseKey key) nKey)
      eRes <- query' impulseState (GetTVSimpleImpulse (buildTestImpulseKey key) 0)
      closeAcidState impulseState
      removeDirectoryRecursive "teststates"
      case eRes of
        Left (ErrorValue ErrorNotFound) -> True `shouldBe` True
        Left a -> expectationFailure $ "Error when accessing key: " ++ (show a)
        Right res -> nKey `shouldBe` res
  describe "DeleteMany" $ do
    it "Should delete a set and then make sure the set is not contained in the resultant set" $ do
      let key = 1
          impulseStore = buildTestImpulseTypeStore key 0 200 [0,1..200] (take 200 [0.2,0.4..])
          nKeys = fromList $ buildNoKeys (take 100 [0..200]) (take 100 [0.2,0.4..])
      impulseState <- openLocalStateFrom "teststates/" (impulseStore)
      update' impulseState (DeleteManyTVSimpleImpulse (buildTestImpulseKey key) nKeys)
      eRes <- query' impulseState (GetTVSimpleImpulseMany (buildTestImpulseKey key) (ImpulseStart 0) (ImpulseEnd 300))
      closeAcidState impulseState
      removeDirectoryRecursive "teststates"
      case eRes of
        Left a -> expectationFailure $ "Error when accessing key: " ++ (show a)
        Right res -> (isSubsetOf nKeys res) `shouldBe` False 


buildNoKeys :: [Integer] -> [Double] -> [TVNoKey]
buildNoKeys is ds = zipWith (\i d -> TVNoKey i d) is ds

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


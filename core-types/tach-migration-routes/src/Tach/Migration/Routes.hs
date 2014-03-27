{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings #-}
module Tach.Migration.Routes where

import Tach.Migration.Routes.Internal
import Tach.Acid.Impulse.State
import Tach.Acid.Impulse.Cruds
import Tach.Impulse.Types.TimeValue
import Tach.Impulse.Types.TimeValueSeries
import Tach.Impulse.Types.Impulse
import Data.Aeson
import Data.Acid
import Data.Acid.Advanced
import Tach.Acid.Impulse.Cruds
import Tach.Migration.Acidic.Types
import Data.Set
import Data.Text
import Data.ByteString.Lazy
import Tach.Impulse.Types.TimeValue
import Network.HTTP.Types

import Tach.Migration.Instances

import Yesod
import Yesod.Core
import Yesod.Core.Types

data MigrationRoutes = MigrationRoutes {
  migrationRoutesAcidPath :: FilePath
 ,migrationRoutesAcid :: AcidState TVSimpleImpulseTypeStore
 ,migrationRoutesTVKey :: TVKey
}

mkYesod "MigrationRoutes" [parseRoutes|
/ HomeR GET
/migration/receive/time-series-data ReceiveTimeSeriesR POST
/list ListDataR GET
/kill KillNodeR GET
|]

instance Yesod MigrationRoutes


  

emptyStore :: TVSimpleImpulseTypeStore
emptyStore = buildTestImpulseTypeStore 0 0 0 [] [] 

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Simple API|]

getListDataR :: Handler Value
getListDataR = do
  master <- getYesod
  eRes <- query' (migrationRoutesAcid master) (GetTVSimpleImpulseMany (buildTestImpulseKey 0) (ImpulseStart 0) (ImpulseEnd 300))
  case eRes of
    Left _ -> return . toJSON $ err
              where err :: Text
                    err = "Error"
    Right res -> return . toJSON $ res


getKillNodeR :: Handler Value
getKillNodeR = do
  master <- getYesod
  liftIO $ closeAcidState (migrationRoutesAcid master)
  return . toJSON $ killing
  where killing :: Text
        killing = "Killing"

--post body -> open state -> add post body to state -> check size (possibly start send)-> close state
--send action
postReceiveTimeSeriesR :: Handler Value
postReceiveTimeSeriesR = do
  rBody <- runRequestBody
  liftIO $ print $ ((\(a,b) -> a) rBody)
  tsInfo <- parseJsonBody :: Handler (Result (Set TVNoKey)) -- Get the post body
  case tsInfo of
    (Error s) -> do
      return . toJSON $ s
    (Success tvSet) -> do
      master <- getYesod
      let impulseState = migrationRoutesAcid master
      --impulseState <- liftIO  $ openLocalStateFrom (migrationRoutesAcidPath master) emptyStore
      update' impulseState (InsertManyTVSimpleImpulse (migrationRoutesTVKey master) tvSet)
      liftIO $ do
        createCheckpoint impulseState
      resultEither (return . toJSON) (\x -> sendResponseStatus (Status 400 (toStrict $ encode x)) (x)) tsInfo

resultEither :: ToJSON a => (a -> c) -> (String -> c) -> Result a -> c
resultEither _ failure (Error s) = failure s
resultEither success _ (Success a) = success a






 --- Just used for testing below this point
buildTestImpulseRep :: [Integer] -> [Double] -> ImpulseRep (Set TVNoKey)
buildTestImpulseRep is ds = ImpulseRep . fromList $ Prelude.zipWith bldFcn is ds 
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



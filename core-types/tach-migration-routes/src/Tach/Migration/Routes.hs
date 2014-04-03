{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Tach.Impulse.Types.TimeValue
import Network.HTTP.Types
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK
import qualified Data.Serialize as S

import GHC.Generics

import Tach.Migration.Instances

import Yesod
import Yesod.Core
import Yesod.Core.Types

data MigrationRoutes = MigrationRoutes {
  migrationRoutesAcidPath :: FilePath
 ,migrationRoutesAcid :: AcidState TVSimpleImpulseTypeStore --Possibly an acid map of acid states
 ,migrationRoutesTVKey :: TVKey                             --A set of TVKeys to handle which PIDs it is responsible for
}

mkYesod "MigrationRoutes" [parseRoutes|
/ HomeR GET
/migration/receive/time-series-data/#String ReceiveTimeSeriesR POST
/list ListDataR GET
/kill KillNodeR GET
|]

instance Yesod MigrationRoutes


testServer = do
  impulseState <- openLocalStateFrom "teststate" (emptyStore)
  warp 3000 (MigrationRoutes "./teststate/" impulseState (buildTestImpulseKey 0))

listTest = do
  impulseState <- openLocalStateFrom "teststate" (emptyStore)
  eRes <- query' impulseState (GetTVSimpleImpulseMany (buildTestImpulseKey 0) (ImpulseStart (-4879536533031178240)) (ImpulseEnd 5364650883968821760))
  closeAcidState impulseState
  case eRes of
    Left _ -> return Data.Set.empty
    Right res -> return res
  

emptyStore :: TVSimpleImpulseTypeStore
emptyStore = buildTestImpulseTypeStore 0 0 0 [] [] 

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Simple API|]

getListDataR :: Handler Value
getListDataR = do
  master <- getYesod
  eRes <- query' (migrationRoutesAcid master) (GetTVSimpleImpulseMany (buildTestImpulseKey 0) (ImpulseStart (-5879536533031178240)) (ImpulseEnd 5364650883968821760))
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

newtype KeyPid = KeyPid { unKeyPid :: Int } deriving (Eq, Show, S.Serialize, Generic)
newtype KeySource = KeySource { unKeySource :: BS.ByteString } deriving (Eq, Show, S.Serialize, Generic)
newtype KeyDestination = KeyDestination { unKeyDestination :: BS.ByteString } deriving (Eq, Show, S.Serialize, Generic)
newtype KeyTime = KeyTime { unKeyTime :: Integer } deriving (Eq, Show, S.Serialize, Generic)
--post body -> open state -> add post body to state -> check size (possibly start send)-> close state
--send action
postReceiveTimeSeriesR :: String -> Handler Value
postReceiveTimeSeriesR stKey = do
  let eDKey = DK.decodeKey (C.pack stKey) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
  tsInfo <- parseJsonBody :: Handler (Result ([TVNoKey])) -- Get the post body
  case tsInfo of
    (Error s) -> do
      liftIO $ Prelude.putStrLn "Failed"
      return . toJSON $ s
    (Success tvSet) -> do
      master <- getYesod
      liftIO $ Prelude.putStrLn . show $ tvSet
      let state = (migrationRoutesAcid master)
      update' state (InsertManyTVSimpleImpulse (migrationRoutesTVKey master) (fromList tvSet))
      liftIO $ do
        createCheckpoint state
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



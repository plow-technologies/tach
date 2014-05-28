{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Tach.Migration.Routes where

--General Haskell imports
import Data.Aeson
import qualified Data.Traversable as T
import Data.ByteString.Lazy
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad
import Data.Foldable
import Data.Text
import Foreign.Storable
import GHC.Generics
import Network.HTTP.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF
import qualified Data.ByteString.Lazy as L
import qualified System.File.Tree as ST
import qualified Network.AWS.S3Simple as S3
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

-- Acid and file related
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local

-- Containers
import qualified Data.Set as S
import qualified Data.Map as M

--External Tach imports
import Tach.Acid.Impulse.State
import Tach.Acid.Impulse.Cruds
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValue
import Tach.Impulse.Types.TimeValueSeries
import Tach.Migration.Acidic.Types
import Tach.Periodic
import Tach.Transformable.Types
import Tach.Migration.Types

-- Yesod and web related
import Yesod
import Yesod.Core
import Yesod.Core.Types

-- Used for serializing and deserializing keys for indexing
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK
import qualified Data.Serialize as S

--Directly related Tach imports
import Tach.Migration.Routes.Internal
import Tach.Migration.Instances

--Wavelets and Compression
import qualified Codec.Compression.GZip as GZ
import Data.Wavelets.Construction

import Tach.Migration.Routes.Types
import Tach.Migration.Foundation

mkYesodDispatch "MigrationRoutes" resourcesMigrationRoutes

instance Yesod MigrationRoutes

-- instance VS.Storable TVNoKey where
 -- sizeOf x = (sizeOf . tvNkSimpleTime $ x) + (sizeOf . tvNkSimpleValue $ x)
instance (ToJSON a) => ToJSON (WaveletTransform a) where
instance (ToJSON a) => ToJSON (PeriodicData a) where
instance (ToJSON a) => ToJSON (APeriodicData a) where


tempS3Conn :: S3.S3Connection
tempS3Conn = S3.S3Connection S3.defaultS3Host "" ""


-- | Used for importing routes into other libraries
--   ONLY
migrationRoutesTransport :: IO MigrationRoutes
migrationRoutesTransport = do
  mMap <- newTVarIO M.empty  :: IO (TVar (M.Map IncomingKey (AcidState TVSimpleImpulseTypeStore)))
  sMap <- newTVarIO (M.empty)
  return $ MigrationRoutes "" mMap (S.empty) tempS3Conn sMap "http://cloud.aacs-us.com"

testServer = do
  let dKey = buildIncomingKey (KeyPid 299) (KeySource "www.aacs-us.com") (KeyDestination "http://cloud.aacs-us.com") (KeyTime 0)
      stateName = C.unpack . DK.parseFilename . DK.encodeKey $ dKey
  impulseState <- openLocalStateFrom stateName emptyStore
  mMap <- newTVarIO (impulseStateMap impulseState dKey)
  sMap <- newTVarIO (M.singleton dKey Idle)
  warp 3000 (MigrationRoutes "./teststate/" mMap (S.singleton . buildTestImpulseKey $ 299) tempS3Conn sMap "http://cloud.aacs-us.com")
  where impulseStateMap state key = M.singleton key state

listTest = do
  impulseState <- openLocalStateFrom "teststate" emptyStore
  eRes <- query' impulseState (GetTVSimpleImpulseMany (buildTestImpulseKey 299) (ImpulseStart (-4879536533031178240)) (ImpulseEnd 5364650883968821760))
  closeAcidState impulseState
  case eRes of
    Left _ -> return S.empty
    Right res -> return res
  

buildIncomingKey :: KeyPid -> KeySource -> KeyDestination -> KeyTime -> IncomingKey
buildIncomingKey pid source dest time = DK.DKeyRaw pid source dest time

emptyStore :: TVSimpleImpulseTypeStore
emptyStore = buildTestImpulseTypeStore (DK.DKeyRaw (KeyPid 299) (KeySource "") (KeyDestination "") (KeyTime 0)) 0 0 [] [] 

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Simple API|]

-- | Gets a list of all data for the specific key
-- should probably be modified to include the start and end times
getListDataR :: String -> Handler Value
getListDataR stKey = do
  master <- getYesod
  migrationMap <- liftIO $ readTVarIO (migrationRoutesAcidMap master)
  let eDKey = DK.decodeKey (C.pack stKey) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      eState = eDKey >>= (\dKey -> maybeToEither "Failed to lookup key" $ M.lookup dKey migrationMap)
      ePidkey = unKeyPid . DK.getSimpleKey <$> eDKey
  eRes <- T.sequence $ (\state pidKey key ->
                          query' state (GetTVSimpleImpulseMany (ImpulseKey key) (ImpulseStart (-5879536533031178240)) (ImpulseEnd 5364650883968821760))) <$>
                            eState <*> ePidkey <*> eDKey
  case eRes of
    Left s -> return . toJSON $ (show s) ++ (show ePidkey)
    Right (Left e) -> return . toJSON . show $ e
    Right (Right res) -> return . toJSON $ res


-- | Kills a node gracefully by closing the acid state
getKillNodeR :: Handler Value
getKillNodeR = do
  master <- getYesod
  migrationMap <- liftIO $ readTVarIO (migrationRoutesAcidMap master)
  let migrationElems = M.elems migrationMap
  let migrationKeys = M.keys migrationMap
  _ <- liftIO $ mapM createCheckpoint migrationElems
  _ <- liftIO $ mapM createArchive migrationElems
  _ <- liftIO $ mapM closeAcidState migrationElems
  directories <- liftIO $ mapM (ST.getDirectory . elemToPath) migrationKeys
  _ <- liftIO $ mapM ST.remove directories
  return . toJSON $ killing
  where killing :: Text
        killing = "Killing"
        elemToPath :: IncomingKey -> FilePath
        elemToPath key = UTF.toString $ BS.append (DK.encodeKey key) "/Archive" --Possibly find a way to make this a little safer?

--post body -> open state -> add post body to state -> check size (possibly start send)-> close state
--send action
postReceiveTimeSeriesR :: String -> Handler Value
postReceiveTimeSeriesR stKey = do
  master <- getYesod
  liftIO $ Prelude.putStrLn stKey
  migrationMap <- liftIO $ readTVarIO (migrationRoutesAcidMap master)
  let eDKey = DK.decodeKey (C.pack stKey) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      eState :: Either String (AcidState TVSimpleImpulseTypeStore)
      eState = eDKey >>= (\dKey -> maybeToEither "Failed to lookup key" $ M.lookup dKey migrationMap)
      ePidKey = unKeyPid . DK.getSimpleKey <$> eDKey
  eTsInfo <- resultToEither <$> parseJsonBody :: Handler (Either String [TVNoKey]) -- Get the post body
  case eDKey of
    Left err -> sendResponseStatus status501 $ toJSON err
    Right dKey -> do
      case eState of
        Left err -> do
          undefined
        Right state -> do
          undefined
  rslt <- T.sequence $ (saveAndUploadState master stKey ) <$> eState <*> eDKey <*> eTsInfo
  sendResponseStatus status200 $ toJSON end
  where end :: String
        end = "Ended"

saveAndUploadState master stKey state dKey tvNkList = do
  let destination = (migrationRoutesDestination master)
  case dKey of
    key@(DK.DKeyRaw {getDest = destination}) -> do
      handleInsert master stKey state dKey (ImpulseKey key) tvNkList
    otherwise -> do
      sendResponseStatus status500 $ toJSON incorrectDestination
      undefined
      where incorrectDestination :: String
            incorrectDestination = "Incorrect destination"



handleInsert :: (MonadHandler m) => MigrationRoutes 
                                      -> String 
                                      -> AcidState (EventState InsertManyTVSimpleImpulse) 
                                      -> (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
                                      -> TVKey
                                      -> [TVNoKey] 
                                      -> m (EventResult InsertManyTVSimpleImpulse)
handleInsert master stKey state dKey key tvSet = do
  res <- update' state (InsertManyTVSimpleImpulse key (S.fromList tvSet))
  eSetSize <- query' state (GetTVSimpleImpulseSize key)
  liftIO . Prelude.putStrLn . show $ eSetSize
  case eSetSize of 
    Left _ -> do
      void $ liftIO $ Prelude.putStrLn "NOT STARTING UPLOAD failed?"
      void $ liftIO $ createCheckpoint state
      sendResponseStatus status501 $ toJSON err
    Right setSize -> do
      if (setSize >= 50000)
        then do
          eBounds <- query' state (GetTVSimpleImpulseTimeBounds key)
          case eBounds of
            (Left _) -> do
              liftIO $ Prelude.putStrLn "Error"
              void $ liftIO $ createCheckpoint state
              sendResponseStatus status501 $ toJSON err
            (Right (ImpulseStart start, ImpulseEnd end)) -> do
              let fName = (show start) ++ "_" ++ (show end)
              void $ liftIO $ Prelude.putStrLn "Started"
              sMap <- liftIO $ readTVarIO (stateMap master)
              case (dKey `M.lookup` sMap) of
                Just Idle -> do
                  liftIO $ atomically $ do
                    writeTVar (stateMap master) (M.insert dKey Uploading sMap)
                  void $ liftIO . forkIO . void $ uploadState master dKey (s3Conn master) state stKey  fName key 15 1 100
                  void $ liftIO $ Prelude.putStrLn "Starting upload"
                  void $ liftIO $ createCheckpoint state
                  sendResponseStatus status201 $ toJSON done
                otherwise -> do
                  void $ liftIO $ Prelude.putStrLn "NOT STARTING UPLOAD"
                  void $ liftIO $ createCheckpoint state
                  sendResponseStatus status201 $ toJSON err
        else do
          void $ liftIO $ Prelude.putStrLn "NOT STARTING UPLOAD"
          void $ liftIO $ createCheckpoint state
          sendResponseStatus status201 $ toJSON err
  sendResponseStatus status501 $ toJSON err
  return res
  where err :: String
        err = "ERROR"
        done :: String
        done = "DONE"

 --classify, compress, upload
uploadState :: MigrationRoutes  
                              -> (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime) 
                              -> S3.S3Connection 
                              -> AcidState (EventState GetTVSimpleImpulseTimeBounds)
                              -> String
                              -> String
                              -> TVKey
                              -> Int
                              -> Int
                              -> Int
                              -> IO (Either String ())
uploadState master dirKey s3Conn state dKey fName key period delta minPeriodicSize = do
  bounds <- query' state (GetTVSimpleImpulseTimeBounds key)
  case bounds of
    (Left _) -> return $ Left "Error retrieving bounds"
    (Right (lower,upper)) -> do
      eSet <- query' state (GetTVSimpleImpulseMany key lower upper)
      case eSet of
        Left _ -> return $ Left "Error retrieving set"
        Right set -> do
          let compressedSet = GZ.compress . encode $ (fmap periodicToTransform) . tvDataToEither <$> (classifySet period delta minPeriodicSize set)
          res <- uploadToS3 s3Conn "testtach" fName dKey compressedSet >>= return . Right
          case res of
            (Right (S3.S3Success _)) -> do
              Prelude.putStrLn "Uploaded to s3! -------||||||||||||------------------------------"
              removeState key set
              sMap <- readTVarIO (stateMap master)
              atomically $ do
                    writeTVar (stateMap master) (M.insert dirKey Uploading sMap)
              return $ Right ()
              where removeState k s = do
                      deleteResult <- update' state (DeleteManyTVSimpleImpulse k s)
                      case deleteResult of
                        Left _ -> removeState k s
                        Right _ -> return . Right $ ()
            (Right (S3.S3Error _)) -> do
              uploadState master dirKey s3Conn state dKey fName key period delta minPeriodicSize
          return . Right $ ()


uploadToS3 :: S3.S3Connection -> String -> String -> String -> L.ByteString -> IO (S3.S3Result ())
uploadToS3 s3Conn bucket filename path contents = do
  let object = S3.S3Object filename (L.toStrict contents) bucket path "text/plain"
  S3.uploadObject s3Conn (S3.S3Bucket bucket "" S3.US) object 

data TimeSeriesQuery = TimeSeriesQuery {
  tsqKey :: String
, tsqStart :: Int
, tsqEnd :: Int
, tsqPeriod :: Int
, tsqDelta :: Int
} deriving (Read, Show, Eq, Generic)

instance FromJSON TimeSeriesQuery where
instance ToJSON TimeSeriesQuery where

postQueryTimeSeriesR :: Handler Value
postQueryTimeSeriesR = do
  eQuery <- resultToEither <$> parseJsonBody :: Handler (Either String TimeSeriesQuery) -- Get the post body
  case eQuery of
    Left err -> sendResponseStatus status501 $ toJSON err
    Right (TimeSeriesQuery key start end period delta) -> do
      master <- getYesod
      migrationMap <- liftIO $ readTVarIO (migrationRoutesAcidMap master)
      let eDKey = DK.decodeKey (C.pack key) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
          eState = eDKey >>= (\dKey -> maybeToEither "Failed to lookup key" $ M.lookup dKey migrationMap)
          ePidkey = unKeyPid . DK.getSimpleKey <$> eDKey
      eRes <- T.sequence $ (\state pidKey key ->
                              query' state (GetTVSimpleImpulseMany (ImpulseKey key) (ImpulseStart start) (ImpulseEnd end))) <$>
                                eState <*> ePidkey <*> eDKey
      case eRes of
        Left s -> sendResponseStatus status501 $ toJSON . show $ s
        Right (Left e) -> sendResponseStatus status501 $ toJSON . show $ e
        Right (Right res) -> do
          let listRes = S.toList res
          if (not . Prelude.null $ listRes)
            then
              sendResponseStatus status201 $ toJSON $ listRes
            else
              sendResponseStatus status201 $ toJSON $ Prelude.foldl (foldPeriod period delta) ([Prelude.head listRes], (Prelude.head listRes) ) (Prelude.tail listRes)
              where foldPeriod period delta (list, lastItem) currItem = 
                      if ((timeDiff >= (period - delta)) && (timeDiff <= (period + delta)))
                        then (list++[currItem],currItem) 
                        else (list,lastItem)
                      where timeDiff = (tvNkSimpleTime currItem - tvNkSimpleTime lastItem)







classifySet :: Int -> Int -> Int -> S.Set TVNoKey -> [TVData TVNoKey]
classifySet period delta minPeriodicSize set = classifyData period delta minPeriodicSize tvNkSimpleTime $ S.toList set


periodicToTransform ::  (PeriodicData TVNoKey) -> (WaveletTransform Double)
periodicToTransform (PeriodicData periodic) = 
  let levels = ceiling ( logBase 2 (fromIntegral . V.length $ periodic))
  in WaveletTransform $ defaultVdwt levels (V.map tvNkSimpleValue periodic)




maybeToEither :: String -> Maybe a -> Either String a
maybeToEither s Nothing = Left s
maybeToEither _ (Just o) = Right o

resultEither :: ToJSON a => (a -> c) -> (String -> c) -> Result a -> c
resultEither _ failure (Error s) = failure s
resultEither success _ (Success a) = success a

resultToEither :: Result a -> Either String a
resultToEither (Error s) = Left . show $ s
resultToEither (Success s) = Right s



testPrint :: IO ()
testPrint = do
  Prelude.putStrLn "RUNNING"

 --- Just used for testing below this point
buildTestImpulseRep :: [Int] -> [Double] -> ImpulseRep (S.Set TVNoKey)
buildTestImpulseRep is ds = ImpulseRep . S.fromList $ Prelude.zipWith bldFcn is ds 
    where 
      bldFcn i d = TVNoKey i d



buildTestImpulseKey :: (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime) -> TVKey
buildTestImpulseKey i = ImpulseKey i 

buildTestImpulseStart :: Int -> TVSStart
buildTestImpulseStart i = ImpulseStart i 

buildTestImpulseEnd :: Int -> TVSEnd
buildTestImpulseEnd i = ImpulseEnd i 


-- | This field is left blank because no period information has been calculated yet so it should stay blank
buildTestImpulsePeriod :: TVPeriod
buildTestImpulsePeriod = initialImpulsePeriod



buildTestImpulseSeries :: (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime) -> Int -> Int -> [Int] -> [Double] 
                       -> ImpulseSeries TVKey TVPeriod TVSStart TVSEnd (ImpulseRep (S.Set TVNoKey))
buildTestImpulseSeries key start end is ds = ImpulseSeries                                 
                                             (buildTestImpulseKey key )                    
                                             (buildTestImpulsePeriod)
                                             (buildTestImpulseStart start)
                                             (buildTestImpulseEnd end )
                                             (buildTestImpulseRep is ds)

buildTestImpulseTypeStore :: (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime) -> Int -> Int -> [Int] -> [Double] 
                          -> TVSimpleImpulseTypeStore 
buildTestImpulseTypeStore key start end is ds = TVSimpleImpulseTypeStore 
                                                (buildTestImpulseSeries key start end is ds)



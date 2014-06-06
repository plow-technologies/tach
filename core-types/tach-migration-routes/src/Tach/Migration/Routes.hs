{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Tach.Migration.Routes where

--General Haskell imports
import Data.Aeson
import qualified Data.Traversable as T
import Data.ByteString.Lazy
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad
import Data.Foldable
import Data.Text
import Foreign.Storable
import GHC.Generics
import Network.HTTP.Types
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF
import qualified Data.ByteString.Lazy as L
import qualified System.File.Tree as ST
import qualified Network.AWS.S3Simple as S3
import qualified Network.AWS.S3SimpleTypes as S3
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Data.Sequence as SEQ
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem as FS
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as OS
--import Filesystem.Path
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

import Data.Acid.Cell.Types

mkYesodDispatch "MigrationRoutes" resourcesMigrationRoutes

instance Yesod MigrationRoutes

-- instance VS.Storable TVNoKey where
 -- sizeOf x = (sizeOf . tvNkSimpleTime $ x) + (sizeOf . tvNkSimpleValue $ x)

instance (ToJSON a ) => ToJSON (SEQ.Seq a) where
  toJSON = toJSON . toList

instance (ToJSON a) => ToJSON (WaveletTransform a) where
instance (ToJSON a) => ToJSON (PeriodicData a) where
instance (ToJSON a) => ToJSON (APeriodicData a) where


tempS3Conn :: S3.S3Connection
tempS3Conn = S3.S3Connection S3.defaultS3Host "" ""

type MigrationTransportTV = MigrationTransport Text TVNoKey


-- | Used for importing routes into other libraries
--   ONLY
-- migrationRoutesTransport :: IO MigrationRoutes
-- migrationRoutesTransport = do
--   mMap <- newTVarIO M.empty  :: IO (TVar (M.Map IncomingKey (AcidState TVSimpleImpulseTypeStore)))
--   sMap <- newTVarIO (M.empty)
--   return $ MigrationRoutes "" mMap (S.empty) tempS3Conn sMap "http://cloud.aacs-us.com"

-- testServer = do
--   let dKey = buildIncomingKey (KeyPid 299) (KeySource "www.aacs-us.com") (KeyDestination "http://cloud.aacs-us.com") (KeyTime 0)
--       stateName = C.unpack . DK.parseFilename . DK.encodeKey $ dKey
--   impulseState <- openLocalStateFrom stateName emptyStore
--   mMap <- newTVarIO (impulseStateMap impulseState dKey)
--   sMap <- newTVarIO (M.singleton dKey Idle)
--   warp 3000 (MigrationRoutes "./teststate/" mMap (S.singleton . buildTestImpulseKey $  (DK.DKeyRaw (KeyPid 299) (KeySource "") (KeyDestination "") (KeyTime 0))) tempS3Conn sMap "http://cloud.aacs-us.com")
--   where impulseStateMap state key = M.singleton key state

listTest = do
  impulseState <- openLocalStateFrom "teststate" emptyStore
  eRes <- query' impulseState (GetTVSimpleImpulseMany (buildTestImpulseKey (DK.DKeyRaw (KeyPid 299) (KeySource "") (KeyDestination "") (KeyTime 0))) (ImpulseStart (-4879536533031178240)) (ImpulseEnd 5364650883968821760))
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
  let acidCell = (migrationRoutesAcidCell master)
      eDKey = DK.decodeKey (C.pack stKey) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      ePidkey = unKeyPid . DK.getSimpleKey <$> eDKey
  eState <- T.sequence $ (\dKey -> liftIO $ attemptLookupInsert acidCell dKey (stateMap master)) <$> eDKey
  eRes <- T.sequence $ (\state pidKey key -> 
                              query' state (GetTVSimpleImpulseMany (ImpulseKey key) (ImpulseStart minBound) (ImpulseEnd maxBound))) <$> eState <*> ePidkey <*> eDKey
  case eRes of
    Left s -> return . toJSON $ (show s) ++ (show ePidkey)
    Right (Left e) -> return . toJSON . show $ e
    Right (Right res) -> return . toJSON $ res

data KeyPidSize = KeyPidSize {
  kpsKey :: T.Text
 ,kpsPid :: Int
 ,kpsSize :: Int
} deriving (Read, Show, Generic)

instance ToJSON KeyPidSize where

getListKeySortedR :: Handler Value
getListKeySortedR = do
  master <- getYesod
  let acidCell = (migrationRoutesAcidCell master)
  res <- liftIO $ traverseWithKeyTVSimpleImpulseTypeStoreAC acidCell (\_ key@(DK.DKeyRaw (KeyPid pid) _ _ _) state -> do
    eSetSize <- query' state (GetTVSimpleImpulseSize (ImpulseKey key))
    return $ eitherToMaybe $ (\size -> KeyPidSize (TE.decodeUtf8 $ DK.encodeKey key) pid size) <$> eSetSize
    )
  return . toJSON . catMaybes $ snd <$> (M.toList res)
  
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left a) = Nothing
eitherToMaybe (Right b) = Just b

showLeft :: (Show a) => Either a b -> Either String b
showLeft (Left s) = Left . show $ s
showLeft (Right a) = Right a

-- | Kills a node gracefully by closing the acid state
getKillNodeR :: Handler Value
getKillNodeR = do
  master <- getYesod
  let acidCell = (migrationRoutesAcidCell master)
      gcStatus = ()
  liftIO $ do
    createCheckpointAndCloseTVSimpleImpulseTypeStoreAC acidCell
    archiveAndHandleTVSimpleImpulseTypeStoreAC acidCell (\ _ state -> return state)
  --     migrationElems = M.elems migrationMap
  --     migrationKeys = M.keys migrationMap
  -- _ <- liftIO $ mapM createCheckpoint migrationElems
  -- _ <- liftIO $ mapM createArchive migrationElems
  -- _ <- liftIO $ mapM closeAcidState migrationElems
  -- directories <- liftIO $ mapM (ST.getDirectory . elemToPath) migrationKeys
  -- _ <- liftIO $ mapM ST.remove directories
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  return . toJSON $ killing
  where killing :: Text
        killing = "Killing"
        elemToPath :: IncomingKey -> FilePath
        elemToPath key = UTF.toString $ BS.append (DK.encodeKey key) "/Archive" --Possibly find a way to make this a little safer?

getStartArchiveR :: Handler Value
getStartArchiveR = do
  master <- getYesod
  let gcState = migrationRoutesGCState master
  liftIO . atomically $ writeTVar gcState GCStart
  checkAndProcessGCState master
  return . toJSON $ (4 :: Int)

checkAndProcessGCState master = do
  let gcState = (migrationRoutesGCState master)
  gcStatus <- liftIO $ readTVarIO gcState
  case gcStatus of
    GCIdle -> do
      liftIO $ print "GC is idle. Continuing as normal"
      return ()
    GCStart -> do
      liftIO $ print "GC is starting!"
      liftIO $ forkIO $ gcAllStates gcState (migrationRoutesAcidCell master) (migrationRoutesStateFP master)
      return ()
    GCRunning -> do
      liftIO $ print "GC already running. Continuing as normal"
      sendResponseStatus status501 $ toJSON running
      return ()
  return ()
  where running :: String
        running = "Running"

getTotalCountR :: Handler Value
getTotalCountR = do
  master <- getYesod
  let acidCell = (migrationRoutesAcidCell master)
  res <- liftIO $ traverseWithKeyTVSimpleImpulseTypeStoreAC acidCell (\_ key@(DK.DKeyRaw (KeyPid pid) _ _ _) state -> do
    eSetSize <- query' state (GetTVSimpleImpulseSize (ImpulseKey key))
    return $ eitherToMaybe $ (\size -> size) <$> eSetSize
    )
  return . toJSON . Prelude.sum . catMaybes $ snd <$> (M.toList res)

gcAllStates :: (MonadIO m) => TVar GCState -> MigrationCell -> T.Text -> m ()
gcAllStates gcState acidCell statesFP = do
  liftIO $ do
    gcR <- readTVarIO gcState
    case gcR of
      GCRunning -> do
        return ()
      _ -> do
        atomically $ writeTVar gcState GCRunning
        dir <- FS.getWorkingDirectory
        archiveAndHandleTVSimpleImpulseTypeStoreAC acidCell (\fp state -> do
          return state)
        asyncRes <- traverseWithKeyTVSimpleImpulseTypeStoreAC acidCell (\_ key state -> async $ (gcSingleState acidCell (dir FP.</> (OS.fromText statesFP)) key state))
        T.traverse wait asyncRes
        atomically $ writeTVar gcState GCIdle
        return ()
    return ()


gcSingleState cellKey dir key state = do
  createCheckpoint state
  createArchive state
  let fullDir = dir FP.</> (OS.fromText . encodeDirectedKeyRaw $ key) FP.</> (OS.fromText "Archive")
  FS.removeTree fullDir
  print fullDir

getCheckGCStateR :: Handler Value
getCheckGCStateR = do
  master <- getYesod
  gcState <- liftIO . readTVarIO $ migrationRoutesGCState master
  return . toJSON $ gcState

--post body -> open state -> add post body to state -> check size (possibly start send)-> close state
--send action
postReceiveTimeSeriesR :: Handler Value
postReceiveTimeSeriesR = do
  master <- getYesod
  checkAndProcessGCState master
  liftIO $ print "1"
  eTsInfo <- resultToEither <$> parseJsonBody :: Handler (Either String [MigrationTransportTV]) -- Get the post body
  liftIO $ print "2"
  let acidCell = (migrationRoutesAcidCell master)
  liftIO $ print "3"
  _ <- T.sequence $ (\l -> Control.Monad.mapM_ (updateMigrationTransports master acidCell) l) <$> eTsInfo -- Update each result that was received
  liftIO $ print "4"
  sendResponseStatus status201 $ toJSON end
  where end :: String
        end = "Ended"


updateMigrationTransports :: MonadHandler m =>
     MigrationRoutes
     -> MigrationCell
     -> MigrationTransportTV
     -> m ()
updateMigrationTransports master acidCell transport = do
  let eDKey@(Right (DK.DKeyRaw (KeyPid p) (KeySource s) (KeyDestination d) (KeyTime t))) = DK.decodeKey (C.pack stKey) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      stKey = T.unpack . key $ transport -- get the possible dKey from the transport
  liftIO $ print "5"
  eState <- T.sequence $ (\dKey -> liftIO $ attemptLookupInsert acidCell dKey (stateMap master)) <$> eDKey -- Get the acid-cell if it exists
  liftIO $ print "6"
  res <- T.sequence $ (\state dKey -> saveAndUploadState master stKey state dKey (tvNkList transport)) <$> eState <*> eDKey -- If there is a state, save and upload the data
  liftIO $ print "7"
  return ()


saveAndUploadState :: MonadHandler m => MigrationRoutes
     -> String
     -> AcidState
          (EventState InsertManyTVSimpleImpulse)
     -> DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
     -> [TVNoKey]
     -> m ()
saveAndUploadState master stKey state dKey tvNkList = do
  let destination = (migrationRoutesDestination master)
  case dKey of
    key@(DK.DKeyRaw {getDest = destination}) -> do
      liftIO $ print "8"
      handleInsert master stKey state (ImpulseKey key) tvNkList
    otherwise -> do
      -- sendResponseStatus status500 $ toJSON incorrectDestination
      return ()
      where incorrectDestination :: String
            incorrectDestination = "Incorrect destination"



-- handleInsert :: (MonadHandler m) => MigrationRoutes 
--                                      -> String 
--                                      -> AcidState (EventState InsertManyTVSimpleImpulse) 
--                                      -> TVKey
--                                      -> [TVNoKey] 
--                                      -> m (EventResult InsertManyTVSimpleImpulse)
handleInsert master stKey state key@(ImpulseKey dKey) tvSet = do
  liftIO $ print "9"
  _ <- update' state (InsertManyTVSimpleImpulse key (S.fromList tvSet)) -- insert the list into the set
  liftIO $ print "10"
  void $ liftIO $ createCheckpoint state
  liftIO $ print "11"
  eSetSize <- query' state (GetTVSimpleImpulseSize key)                 -- get the set size and bounds
  liftIO $ print "12"
  eBounds <- query' state (GetTVSimpleImpulseTimeBounds key)
  liftIO . Prelude.putStrLn $ ("---------------------------------------------------------------------" :: String) ++ (show eSetSize) ++ (show . DK.encodeKey $ dKey)
  liftIO $ print "13"
  res <- T.sequence $ (\size bounds -> checkAndUpload master state stKey key size bounds) <$> eSetSize <*> eBounds
  case res of
    Left _ -> do
      -- sendResponseStatus status500 $ toJSON err
      return ()
    Right suc -> do
      -- sendResponseStatus status201 $ toJSON done
      return ()
  return ()
  where err :: String
        err = "ERROR"
        done :: String
        done = "DONE"


checkAndUpload master state stKey key@(ImpulseKey dKey) size (ImpulseStart start, ImpulseEnd end) = do
  liftIO . Prelude.putStrLn . show $ size
  if (size >= 50000)
    then do
      sMap <- liftIO . atomically $ readTMVar tmMap
      case (dKey `M.lookup` sMap) of
        Just Idle -> do
          liftIO $ atomically $ do
            tsMap <- takeTMVar tmMap
            putTMVar tmMap (M.insert dKey Uploading tsMap)
          void $ liftIO . forkIO . void $ uploadState master (s3Conn master) state stKey fName key 15 1 100
          void $ liftIO $ Prelude.putStrLn "Starting upload"
          void $ liftIO $ createCheckpoint state
          -- sendResponseStatus status201 $ toJSON done
        otherwise -> do
          void $ liftIO $ Prelude.putStrLn "NOT STARTING UPLOAD"
          void $ liftIO $ createCheckpoint state
          -- sendResponseStatus status201 $ toJSON err
    else do
      void $ liftIO $ Prelude.putStrLn "NOT STARTING UPLOAD"
      void $ liftIO $ createCheckpoint state
      -- sendResponseStatus status201 $ toJSON err
  where
    fName = (show start) ++ "_" ++ (show end)
    err :: String
    err = "Error"
    done :: String
    done = "Done"
    tmMap = stateMap master


 --classify, compress, upload
uploadState :: MigrationRoutes  
                              -> S3.S3Connection 
                              -> AcidState (EventState GetTVSimpleImpulseTimeBounds)
                              -> String
                              -> String
                              -> TVKey
                              -> Int
                              -> Int
                              -> Int
                              -> IO (Either String ())
uploadState master s3Conn state stKey fName key@(ImpulseKey dKey) period delta minPeriodicSize = do
  bounds <- query' state (GetTVSimpleImpulseTimeBounds key)
  case bounds of
    (Left _) -> return $ Left "Error retrieving bounds"
    (Right (lower,upper)) -> do
      eSet <- query' state (GetTVSimpleImpulseMany key lower upper)
      case eSet of
        Left _ -> return $ Left "Error retrieving set"
        Right set -> do
          let compressedSet = GZ.compress . encode $ (fmap periodicToTransform) . tvDataToEither <$> (classifySet period delta minPeriodicSize set)
          res <- uploadToS3 s3Conn (migrationRoutesS3Bucket master) fName stKey compressedSet >>= return . Right
          case res of
            (Right (S3.S3Success _)) -> do
              Prelude.putStrLn "Uploaded to s3! -------||||||||||||------------------------------"
              removeState key set
              atomically $ do
                  tsMap <- takeTMVar (stateMap master)
                  putTMVar (stateMap master) (M.insert dKey Idle tsMap)
              return $ Right ()
              where removeState k s = do
                      deleteResult <- update' state (DeleteManyTVSimpleImpulse k s)
                      createCheckpoint state
                      case deleteResult of
                        Left _ -> do
                          Prelude.putStrLn "NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT NOT  -------------------------------------------//////////////////////0000000000000"
                          removeState k s
                        Right _ -> do
                          Prelude.putStrLn "DELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETEDDELETED //////////////////////////////***************************************\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
                          return . Right $ ()
            (Right (S3.S3Error _)) -> do
              uploadState master s3Conn state stKey fName key period delta minPeriodicSize
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
  master <- getYesod
  eQuery <- resultToEither <$> parseJsonBody :: Handler (Either String TimeSeriesQuery) -- Get the post body
  case eQuery of
    Left err -> sendResponseStatus status501 $ toJSON err
    Right (TimeSeriesQuery key start end period delta) -> do
      master <- getYesod
      let acidCell = (migrationRoutesAcidCell master)
          eDKey = DK.decodeKey (C.pack key) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      eState <- T.sequence $ (\key -> liftIO $ attemptLookupInsert acidCell key (stateMap master)) <$> eDKey
      let ePidkey = unKeyPid . DK.getSimpleKey <$> eDKey
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


attemptLookupInsert cell key tmMap = do
  let store = createTVSimpleStoreFromKey key 
  mRes <- getTVSimpleImpulseTypeStoreAC cell store
  case mRes of
    Just res -> return res
    Nothing -> do
      st <- insertTVSimpleImpulseTypeStoreAC cell store
      updateTVSimpleImpulseTypeStoreAC cell st store
      atomically $ do
        tsMap <- takeTMVar tmMap
        putTMVar tmMap (M.insert key Idle tsMap)
      createCheckpoint st
      return st




classifySet :: Int -> Int -> Int -> S.Set TVNoKey -> [TVData TVNoKey]
classifySet period delta minPeriodicSize set = classifyData period delta minPeriodicSize tvNkSimpleTime $ S.toList set


periodicToTransform ::  (PeriodicData TVNoKey) -> (WaveletTransform Double)
periodicToTransform (PeriodicData periodic) = 
  let levels = ceiling ( logBase 2 (fromIntegral . SEQ.length $ periodic))
  in WaveletTransform $ defaultVdwt levels (toList $ fmap tvNkSimpleValue periodic)




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




{-# LANGUAGE QuasiQuotes, TemplateHaskell, RecordWildCards, DeriveGeneric, OverloadedStrings, ViewPatterns #-}

-- | 'YesodDispatch' instance of 'MigrationRoutes' type.
module Tach.Migration.Routes () where

-- General Haskell imports
import qualified Control.Exception as E
import Data.Aeson hiding (encode)
import qualified Data.Traversable as T
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.Async.Lifted as AL
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad
import Data.Foldable (toList)
import GHC.Generics
import Network.HTTP.Types
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF
import qualified Data.ByteString.Lazy as L
import qualified Network.AWS.S3Simple as S3
import qualified Network.AWS.S3SimpleTypes as S3
import Data.Text (Text,unpack)
import qualified Data.Text.Encoding as TE
import qualified Filesystem as FS
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as OS
-- Acid and file related
import Data.Acid  (AcidState, EventState
                 , createArchive, createCheckpoint)
import Data.Acid.Advanced (query', update')
import Data.Acid.Cell (AcidCell,InsertAcidCellPathFileKey)
-- Containers
import qualified Data.Set as S
import qualified Data.Map as M

-- External Tach imports
import Tach.Acid.Impulse.Cruds
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValue
import Tach.Migration.Acidic.Types
-- import Tach.Periodic
import Tach.Migration.Types
import Control.Monad.IO.Class
-- Yesod and web related
import Yesod

-- Used for serializing and deserializing keys for indexing
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK

-- Directly related Tach imports
import Tach.Migration.Routes.Internal
import Tach.Migration.Instances()

import Tach.Migration.Routes.Types
import Tach.Migration.Foundation

import qualified Data.Serialize as SER
import Data.Either

-- Binary List and Store
import qualified Data.BinaryList as BL
import Format.BinaryStore

mkYesodDispatch "MigrationRoutes" resourcesMigrationRoutes

instance Yesod MigrationRoutes where --changing default methods for larger upload sizes
  maximumContentLength _ (Just (ReceiveTimeSeriesR _)) = Just $ 2 * 1024 * 1024 * 1024 -- 2 gigabytes for ReceiveTimeSeries
  maximumContentLength _  _ = Just $ 2  * 1024 * 1024 -- 2 mb 

type MigrationTransportTV = MigrationTransport Text TVNoKey

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Simple API|]

-- | Gets a list of all data for the specific key
-- should probably be modified to include the start and end times
getListDataR :: String -> Handler Value
getListDataR stKey = do
  master <- getYesod
  let acidCell = migrationRoutesAcidCell master
      eDKey = DK.decodeKey (C.pack stKey) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      ePidkey = unKeyPid . DK.getSimpleKey <$> eDKey
  eState <- T.sequence $ (\dKey -> liftIO $ attemptLookupInsert acidCell dKey (stateMap master)) <$> eDKey
  eRes <- T.sequence $ (\state key -> 
                              query' state (GetTVSimpleImpulseMany (ImpulseKey key) (ImpulseStart minBound) (ImpulseEnd maxBound))) <$> eState <*> eDKey
  case eRes of
    Left s -> return . toJSON $ show s ++ show ePidkey
    Right (Left e) -> return . toJSON . show $ e
    Right (Right res) -> return . toJSON $ res

data KeyPidSize =
  KeyPidSize
     Text -- Key
     Int -- Pid
     Int -- Size
       deriving (Read, Show, Generic)

instance ToJSON KeyPidSize where

getListKeySortedR :: Handler Value
getListKeySortedR = do
  master <- getYesod
  let acidCell = migrationRoutesAcidCell master
  res <- liftIO $ traverseWithKeyTVSimpleImpulseTypeStoreAC acidCell (\_ key@(DK.DKeyRaw (KeyPid pid) _ _ _) state -> do
    eSetSize <- query' state $ GetTVSimpleImpulseSize $ ImpulseKey key
    return $ eitherToMaybe $ KeyPidSize (TE.decodeUtf8 $ DK.encodeKey key) pid <$> eSetSize
    )
  return . toJSON . catMaybes $ snd <$> M.toList res
  
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

-- | Kills a node gracefully by closing the acid state
getKillNodeR :: Handler Value
getKillNodeR = do
  master <- getYesod
  let acidCell = migrationRoutesAcidCell master
  liftIO $ do
    --archiveAndHandleTVSimpleImpulseTypeStoreAC acidCell (\ _ state -> return state)
    createCheckpointAndCloseTVSimpleImpulseTypeStoreAC acidCell
  --     migrationElems = M.elems migrationMap
  --     migrationKeys = M.keys migrationMap
  -- _ <- liftIO $ mapM createCheckpoint migrationElems
  -- _ <- liftIO $ mapM createArchive migrationElems
  -- _ <- liftIO $ mapM closeAcidState migrationElems
  -- directories <- liftIO $ mapM (ST.getDirectory . elemToPath) migrationKeys
  -- _ <- liftIO $ mapM ST.remove directories

  -- Why is this operation done four times? (Also: replicateM_ exists)
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  void . liftIO $ tryPutMVar (migrationRoutesWait master) 0
  return $ toJSON ("Killing" :: Text)

getStartArchiveR :: Handler Value
getStartArchiveR = do
  master <- getYesod
  let gcState = migrationRoutesGCState master
  gc <- liftIO . atomically $ readTVar gcState
  case gc of
    GCIdle -> do
      liftIO . atomically $ writeTVar gcState GCStart
      checkAndProcessGCState master
      return . toJSON $ ("Starting GC" :: String)
    _ -> do
      return . toJSON $ gc

checkAndProcessGCState :: MonadHandler m => MigrationRoutes -> m ()
checkAndProcessGCState master = do
  let gcState = migrationRoutesGCState master
  gcStatus <- liftIO $ readTVarIO gcState
  case gcStatus of
    GCIdle -> do
      --liftIO $ print "GC is idle. Continuing as normal"
      return ()
    GCStart -> do
      liftIO $ Prelude.putStrLn "GC is starting!"
      void $ liftIO $ forkIO $ gcAllStates gcState (migrationRoutesAcidCell master) (migrationRoutesStateFP master)
    GCRunning -> do
      liftIO $ Prelude.putStrLn "GC already running. Not starting anything."
      void $ sendResponseStatus status501 $ toJSON ("Running" :: String)

getTotalCountR :: Handler Value
getTotalCountR = do
  master <- getYesod
  let acidCell = (migrationRoutesAcidCell master)
  res <- liftIO $ traverseWithKeyTVSimpleImpulseTypeStoreAC acidCell (\_ key state -> do
    eSetSize <- query' state (GetTVSimpleImpulseSize (ImpulseKey key))
    case eSetSize of
      Left  _ -> liftIO $ print . DK.encodeKey $ key
      Right _ -> return ()
    return eSetSize
    )
  let rightsList = rights $ snd <$> (M.toList res)
      leftsList = lefts $ snd <$> (M.toList res)
  if Prelude.length rightsList /= Prelude.length (M.toList res)
    then do
      liftIO . Prelude.putStrLn . show $ leftsList
      return . toJSON $ (-1 :: Int)
    else do
      liftIO . Prelude.putStrLn . show $ leftsList
      return . toJSON . Prelude.sum . rights $ snd <$> (M.toList res)

gcAllStates :: (MonadIO m) => TVar GCState -> MigrationCell -> Text -> m ()
gcAllStates gcState acidCell statesFP = do
  liftIO $ do
    gcR <- readTVarIO gcState
    case gcR of
      GCRunning -> do
        return ()
      _ -> do
        atomically $ writeTVar gcState GCRunning
        dir <- FS.getWorkingDirectory
        liftIO . Prelude.putStrLn $ "GC acid-cell archiving"
        --archiveAndHandleTVSimpleImpulseTypeStoreAC acidCell (\_ state -> do
        --  return state)
        liftIO . Prelude.putStrLn $ "GC traversing"
        let dirToStates = dir FP.</> (OS.fromText statesFP)
        stateKeyList <- traverseWithKeyTVSimpleImpulseTypeStoreAC acidCell (\_ key _ -> async $ gcSingleState dirToStates key)
        liftIO . Prelude.putStrLn $ "GC waiting"
        _ <- T.traverse wait stateKeyList
        liftIO . Prelude.putStrLn $ "GC done"
        atomically $ writeTVar gcState GCIdle

gcSingleState :: (SER.Serialize datetime, SER.Serialize destination, SER.Serialize source, SER.Serialize key)
              => OS.FilePath -> DK.DirectedKeyRaw key source destination datetime -> IO ()
gcSingleState dir key = do
  let fullDir = dir FP.</> (OS.fromText . encodeDirectedKeyRaw $ key) FP.</> OS.fromText "Archive"
  print fullDir
  isDir <- FS.isDirectory fullDir
  when isDir $ FS.removeTree fullDir

-- | returns the gc state
getCheckGCStateR :: Handler Value
getCheckGCStateR = do
  master <- getYesod
  gcState <- liftIO . readTVarIO $ migrationRoutesGCState master
  return . toJSON $ gcState

--post body -> open state -> add post body to state -> check size (possibly start send)-> close state
--send action
postReceiveTimeSeriesR :: Int -> Handler Value
postReceiveTimeSeriesR size = do
  master <- getYesod
  checkAndProcessGCState master
  liftIO $ Prelude.putStrLn "Parsing JSON body"
  eTsInfo <- resultToEither <$> parseJsonBody :: Handler (Either String [MigrationTransportTV]) -- Get the post body
  case eTsInfo of
    Left err -> do
      liftIO $ Prelude.putStrLn $
            "[tach-migration-routes] postReceiveTimeSeriesR: error parsing JSON body.\n"
         ++ "    The error was:\n"
         ++ unlines (fmap ("    " ++) $ lines err)
      sendResponseStatus status501 $ toJSON ("Error parsing JSON body" :: String)
    Right tsInfo -> do
      let gs = groupUp 16 tsInfo
          acidCell = migrationRoutesAcidCell master
      liftIO $ Prelude.putStrLn "Traversing"
      res <- mapM (\smallList -> do
                       -- Every 'updateMigrationTransports' is executed in a separate thread
                       asList <- T.traverse (AL.async . updateMigrationTransports master acidCell) smallList
                       -- Then we wait for all the threads to end and catch any exception of any
                       -- thread by returning a @Left SomeException@ value.
                       T.traverse AL.waitCatch asList
                         ) gs
      liftIO $ Prelude.putStrLn "Returning!"
      let errs = lefts $ Prelude.concat res
      if Prelude.null errs
         then do liftIO $ Prelude.putStrLn "Success. 0 failures."
                 sendResponseStatus status201 $ toJSON size
         else do liftIO $ do
                   Prelude.putStrLn "[tach-migration-routes] postReceiveTimeSeriesR: at least one thread returned a exception."
                   Prelude.putStrLn "    Exceptions were:"
                   Control.Monad.mapM_ (\err -> Prelude.putStrLn
                                  $ "    ** " ++ show err) errs
                   Prelude.putStrLn "    Sending 501 Status response."
                 sendResponseStatus status501 $ toJSON ("Error!" :: String)

updateMigrationTransports :: MonadHandler m =>
     MigrationRoutes
     -> MigrationCell
     -> MigrationTransportTV
     -> m ()
updateMigrationTransports master acidCell transport = do
  let eDKey :: Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
      eDKey = DK.decodeKey $ C.pack stKey 
      stKey = unpack . key $ transport -- get the possible dKey from the transport
  case eDKey of
    Right k -> do
      st <- liftIO $ attemptLookupInsert acidCell k $ stateMap master -- Get the acid-cell if it exists
      _ <- saveAndUploadState master stKey st k $ tvNkList transport -- If there is a state, save and upload the data
      return ()
    _ -> liftIO $ Prelude.putStrLn "[tach-migration-routes] updateMigrationTransports: Error decoding key."

saveAndUploadState :: MonadHandler m =>
                            MigrationRoutes
                            -> String
                            -> AcidState (EventState GetTVSimpleImpulseSize)
                            -> DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
                            -> [TVNoKey]
                            -> m Bool
saveAndUploadState master stKey state dKey tvNkList = do
  let destination = migrationRoutesDestination master
  if destination == (UTF.toString . unKeyDestination . DK.getDest) dKey
    then handleInsert master stKey state (ImpulseKey dKey) tvNkList --If the destination is the same as the destination for the current host
    else do
      _ <- sendResponseStatus status501 $ toJSON incorrectDestination
      return False
      where incorrectDestination :: String
            incorrectDestination = "Incorrect destination"

handleInsert :: (MonadIO m, Functor m) =>
                      MigrationRoutes
                      -> String
                      -> AcidState (EventState GetTVSimpleImpulseSize)
                      -> ImpulseKey
                           (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
                      -> [TVNoKey]
                      -> m Bool
handleInsert master stKey state key tvSet = do
  ePreSetSize <- query' state $ GetTVSimpleImpulseSize key
  void $ update' state $ InsertManyTVSimpleImpulse key $ S.fromList tvSet -- insert the list into the set
  eSetSize <- query' state $ GetTVSimpleImpulseSize key                   -- get the set size and bounds
  void . liftIO . T.sequence $ checkCreateCheckpoint state <$> ePreSetSize <*> eSetSize
  eBounds <- query' state $ GetTVSimpleImpulseTimeBounds key
  -- Check and upload state
  res <- liftIO $ T.sequence $ checkAndUpload master state stKey key <$> eSetSize <*> eBounds
  return $ isRight res

-- | Used only after insert. Checks the two sizes and if they are equal it creates a checkpoint and archives it
--   hopefully it cuts down on the size of archives
checkCreateCheckpoint :: Eq a => AcidState st -> a -> a -> IO ()
checkCreateCheckpoint state preSize postSize =
  unless (preSize == postSize) $ E.finally (createArchive state) (createCheckpoint state)

checkAndUpload ::
     MigrationRoutes
     -> AcidState (EventState GetTVSimpleImpulseTimeBounds)
     -> String
     -> ImpulseKey IncomingKey
     -> Int
     -> (ImpulseStart Int, ImpulseEnd Int)
     -> IO ()
checkAndUpload master state stKey key@(ImpulseKey dKey) size bounds@(ImpulseStart start, ImpulseEnd end) = do
  Prelude.putStrLn $ "checkAndUpload size: " ++ show size
  when (size >= 50000) $ do
    sMap <- atomically $ readTMVar tmMap
    case dKey `M.lookup` sMap of
      Just Idle -> do
        atomically $ do
          tsMap <- takeTMVar tmMap
          putTMVar tmMap $ M.insert dKey Uploading tsMap
        -- Old version call, with period information arguments
        -- void $ liftIO . forkIO . void $ uploadState master (s3Conn master) state stKey fName key 15 1 100 bounds
        _ <- forkIO . void $ uploadState master (s3Conn master) state stKey fName key bounds
        Prelude.putStrLn "Starting upload"
        E.finally (createArchive state) (createCheckpoint state)
      _ -> return ()
  where
    fName = show start ++ '_' : show end
    tmMap = stateMap master

uploadState :: MigrationRoutes
                     -> S3.S3Connection
                     -> AcidState (EventState DeleteManyTVSimpleImpulse)
                     -> String
                     -> String
                     -> ImpulseKey
                          (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
                     -- -> Int
                     -- -> Int
                     -- -> Int
                     -> (ImpulseStart Int, ImpulseEnd Int)
                     -> IO ()
uploadState master s3Conn state stKey fName key@(ImpulseKey dKey) {- period delta minPeriodicSize -} (start,end) = do
  Prelude.putStrLn "[tach-migration-routes] uploadState: started."
  eSet <- query' state $ GetTVSimpleImpulseMany key start end
  res <- T.sequence $ (\set -> do
          let (_,vs) = normalizeSampling $ toList set
              binaryStore = encode $ createBinaryStoreDefault $ BL.fromListWithDefault Nothing vs
          r <- uploadToS3 s3Conn (migrationRoutesS3Bucket master) fName stKey binaryStore >>= return . Right
          case r of
            Right (S3.S3Success _) -> do
              Prelude.putStrLn "Uploaded to S3"
              _ <- removeState key set
              atomically $ do
                tsMap <- takeTMVar $ stateMap master
                putTMVar (stateMap master) $ M.insert dKey Idle tsMap
              return $ Just ()
            _ -> do
              atomically $ do
                tsMap <- takeTMVar $ stateMap master
                putTMVar (stateMap master) $ M.insert dKey Idle tsMap
              return Nothing) <$> eitherToMaybe eSet
  case res of
    Nothing -> Prelude.putStrLn "[tach-migration-routes] uploadState: Error uploading state."
    _ -> return ()
  where removeState k s = do
          deleteResult <- update' state $ DeleteManyTVSimpleImpulse k s
          -- liftIO $ createCheckpoint state
          liftIO $ E.finally (createArchive state) (createCheckpoint state)
          case deleteResult of
            Left _ -> do
              removeState k s
            _ -> do
              Prelude.putStrLn "Deleted set from state"
              return . Right $ ()

uploadToS3 :: S3.S3Connection -> String -> String -> String -> L.ByteString -> IO (S3.S3Result ())
uploadToS3 s3Conn bucket filename path contents =
  S3.uploadObject s3Conn (S3.S3Bucket bucket "" S3.US) $
     S3.S3Object filename (L.toStrict contents) bucket path "text/plain"

data TimeSeriesQuery =
  TimeSeriesQuery
    String -- Key
    Int    -- Start
    Int    -- End
    Int    -- Period
    Int    -- Delta
      deriving (Read, Show, Eq, Generic)

instance FromJSON TimeSeriesQuery where

postQueryTimeSeriesR :: Handler Value
postQueryTimeSeriesR = do
  liftIO $ Prelude.putStrLn "[postQueryTimeSeriesR] started."
  eQuery <- resultToEither <$> parseJsonBody :: Handler (Either String TimeSeriesQuery) -- Get the post body
  case eQuery of
    Left err -> sendResponseStatus status501 $ toJSON err
    Right (TimeSeriesQuery key start end period delta) -> do
      master <- getYesod
      let acidCell = migrationRoutesAcidCell master
          eDKey = DK.decodeKey (C.pack key) :: (Either String (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime))
      eState <- T.sequence $ (\k -> liftIO $ attemptLookupInsert acidCell k $ stateMap master) <$> eDKey
      let ePidkey = unKeyPid . DK.getSimpleKey <$> eDKey
      eRes <- T.sequence $ (\state _ k ->
                              query' state $ GetTVSimpleImpulseMany (ImpulseKey k) (ImpulseStart start) (ImpulseEnd end)
                             ) <$> eState <*> ePidkey <*> eDKey
      case eRes of
        Left s -> sendResponseStatus status501 $ toJSON . show $ s
        Right (Left e) -> sendResponseStatus status501 $ toJSON . show $ e
        Right (Right res) -> do
          let listRes = S.toList res
              h:t = listRes
          sendResponseStatus status201 $
            if not . Prelude.null $ listRes
               then toJSON $ listRes
               else toJSON $ Prelude.foldl (foldPeriod period delta) ([h] , h) t

foldPeriod :: Int -> Int -> ([TVNoKey], TVNoKey) -> TVNoKey -> ([TVNoKey], TVNoKey)
foldPeriod period delta (list,lastItem) currItem =
  let timeDiff = tvNkSimpleTime currItem - tvNkSimpleTime lastItem
  in  if timeDiff >= (period - delta) && timeDiff <= (period + delta)
         then (list ++ [currItem] , currItem) 
         else (list,lastItem)

attemptLookupInsert :: AcidCell KeyPid KeySource KeyDestination KeyTime TVSimpleImpulseTypeStore
                                (AcidState (EventState InsertAcidCellPathFileKey))
                             -> DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
                             -> TMVar
                                  (M.Map
                                     (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
                                     StateStatus)
                             -> IO (AcidState TVSimpleImpulseTypeStore)
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
        putTMVar tmMap $ M.insert key Idle tsMap
      E.finally (createArchive st) (createCheckpoint st)
      return st

-- Sample list normalization

normalPeriod :: Int
normalPeriod = 1

normalizeSampling :: [TVNoKey] -> (Int,[Maybe Double])
normalizeSampling [] = (0,[])
normalizeSampling vs =
  let -- First, we scale time so a unit of time is equivalent to normalPeriod.
      -- The results are stored in pairs, to feed them later to Data.Map.fromList.
      nvs = fmap (\(TVNoKey t x) -> (div t normalPeriod,x)) vs
      -- We place time scaled values in ascendent order, getting rid of repetitions
      -- in time.
      ws = fmap (uncurry TVNoKey) $ M.toAscList $ M.fromListWith (\x y -> (x+y)/2) nvs
      -- Let t0 (initial time) be the scaled time of the first entry
      t0 = tvNkSimpleTime $ Prelude.head ws
      -- Fill every unit of scaled time, either with Just or Nothing, depending if
      -- there is a value in that particular time. This function makes some assumptions.
      -- Namely:
      --  * Time of the keys are in ascendent order.
      --  * There are no two keys with the same scaled time.
      -- These two conditions are guaranteed by construction of ws.
      go ct xs@(TVNoKey t x : ys) =
        if t == ct
           then Just x  : go (ct+1) ys
           else Nothing : go (ct+1) xs
      go _ [] = []
  in  ( t0*normalPeriod -- Normalized start time
      , go t0 ws        -- List of (maybe) samples
        )

---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

resultToEither :: Result a -> Either String a
resultToEither (Error s) = Left . show $ s
resultToEither (Success s) = Right s


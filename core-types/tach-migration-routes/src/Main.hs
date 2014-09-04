
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

module Main where

import GHC.Generics
import Control.Applicative
import System.Console.CmdArgs hiding (args)
import Control.Monad
import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import qualified Data.Text as T
import Yesod.Core
import qualified Data.Set as S
import qualified Data.Map as M
import Network.Wai.Handler.Warp
import qualified Filesystem.Path.CurrentOS as OS

-- Acid and file related
import Data.Acid
import Control.Concurrent
import Tach.Migration.Foundation
import Tach.Migration.Routes.Types
import Control.Exception
import Data.Yaml 
import Data.Streaming.Network.Internal

import qualified Filesystem as FS

-- Instances
import Tach.Migration.Routes()

data MigrationPath = MigrationPath{ 
   migrationPath :: String
} deriving (Data, Typeable, Show, Eq)

--s3Conn :: S3Connection
--s3Conn = S3Connection defaultS3Host "AKIAI5PX6WURXC7EAEWA" "n+l3EqtsdVwPidtOZ++l/CdK/cJzrAmTih+O9JFi"


main :: IO ()
main = do
  args <- cmdArgs $ MigrationPath "config.yml"
  let fullPath = OS.fromText $ T.pack $ migrationPath args
  print args
  file <- FS.readFile fullPath
  let eConf = Y.decodeEither file
  case eConf of
    Left _ -> putStrLn "Error reading config file"
    Right theConf -> do
      migrationConfig <- readMigrationConfig fullPath
      let runServer conf migrationConf = do
            let -- dKey = buildIncomingKey (KeyPid 300) (KeySource "www.aacs-us.com") (KeyDestination "http://cloud.aacs-us.com") (KeyTime 0)
                -- impulseStateMap = flip M.singleton
            cells <- initializeTVSimpleImpulseTypeStoreAC (migrationStatePath migrationConf)
            resMap <- foldlWithKeyTVSimpleImpulseTypeStoreAC cells (\_ key _ ioStates -> M.insert key Idle <$> ioStates) (return M.empty)
            sMap <- newTMVarIO resMap
            wait <- newEmptyMVar
            gcState <- newTVarIO GCIdle
            _ <- forkIO . void $ gcRunner (migrationGCDelayMinutes migrationConf * 60 * 1000 * 1000) gcState
            _ <- forkIO $ notWarpWarp migrationConf
                                      (MigrationRoutes cells S.empty conf sMap "http://cloud.aacs-us.com" wait (migrationS3Bucket migrationConf) (migrationStatePath migrationConf) gcState)
                                      wait
            void $ takeMVar wait
      runServer theConf migrationConfig

-- createWarp port cells keys conn sMap host wait = do
--   warp 3000 (MigrationRoutes cells (S.singleton . buildTestImpulseKey $ dKey) conn sMap "http://cloud.aacs-us.com" wait)

notWarpWarp :: MigrationConfig -> MigrationRoutes -> MVar a -> IO ()
notWarpWarp config app wait = do
  finally (do putStrLn $ "Starting migration server"
              wapp <- toWaiApp app
              _ <- startServer wapp config 
              res <- takeMVar wait
              _ <- putMVar wait res
              return ()
           )
          (void $ do
              putStrLn ("Closing migration server" :: String)
              let cells = migrationRoutesAcidCell app
              void $ do
                _ <- archiveAndHandleTVSimpleImpulseTypeStoreAC cells (\_ b -> createCheckpoint b >> return b)
                createCheckpointAndCloseTVSimpleImpulseTypeStoreAC cells
            )

startServer :: Application -> MigrationConfig -> IO ()
startServer app conf = runSettings warpSettings app
  where
    warpSettings = setFdCacheDuration 0 . setTimeout 120 . setHost (Host $ T.unpack $ migrationHost conf) . (setPort . migrationPort $ conf) $ defaultSettings

data MigrationConfig = MigrationConfig { 
      migrationPort :: Int 
    , migrationHost :: T.Text
    , migrationStatePath :: T.Text
    , migrationS3Bucket :: String
    , migrationGCDelayMinutes :: Int
} deriving (Read, Eq, Show, Typeable,Generic)

readMigrationConfig :: OS.FilePath -> IO MigrationConfig
readMigrationConfig fPath = do
  fCont <- BS.readFile $ OS.encodeString fPath
  either fail return $ decodeEither $ fCont

instance FromJSON MigrationConfig where
instance ToJSON MigrationConfig where

gcRunner :: Int -> TVar GCState -> IO b
gcRunner delay tv = forever $ do
  threadDelay delay
  putStrLn "Attempting to start garbage collection"
  atomically $ writeTVar tv GCStart
  waitTVar tv

waitTVar :: TVar GCState -> IO ()
waitTVar tv = do
  status <- atomically $ readTVar tv
  case status of
    GCIdle -> return ()
    _ -> do
      threadDelay 10000000 -- 10 seconds
      waitTVar tv

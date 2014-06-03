{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import Control.Monad
import Network.AWS.S3Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Either as E
import qualified Data.Yaml as Y
import qualified Data.Text as T
import Yesod.Core
import Yesod
import qualified Data.Set as S
import qualified Data.Map as M
import Tach.Migration.Routes
import Tach.Migration.Types
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK

-- Acid and file related
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Concurrent.MVar
import Tach.Migration.Foundation
import Tach.Migration.Routes.Types
import Control.Exception

data MigrationConfig = MigrationConfig{ 
   path :: String
} deriving (Data, Typeable, Show, Eq)

--s3Conn :: S3Connection
--s3Conn = S3Connection defaultS3Host "AKIAI5PX6WURXC7EAEWA" "n+l3EqtsdVwPidtOZ++l/CdK/cJzrAmTih+O9JFi"


main :: IO ()
main = do
  args <- cmdArgs $ MigrationConfig "config.yml"
  putStrLn $ show args
  file <- BS.readFile $ path args
  let eConf =  Y.decodeEither file
  case eConf of
    (Left _) -> putStrLn "Error reading config file"
    (Right conf) -> do
      runServer conf
      where runServer conn = do
              let dKey = buildIncomingKey (KeyPid 300) (KeySource "www.aacs-us.com") (KeyDestination "http://cloud.aacs-us.com") (KeyTime 0)
              sMap <- newTMVarIO (M.singleton dKey Idle)
              cells <- initializeTVSimpleImpulseTypeStoreAC "states"
              st <- insertTVSimpleImpulseTypeStoreAC cells initTVSimpleStore
              updateTVSimpleImpulseTypeStoreAC cells st initTVSimpleStore
              createCheckpoint st
              wait <- newEmptyMVar
              forkIO $ finally () $ warp 3000 (MigrationRoutes cells (S.singleton . buildTestImpulseKey $ dKey) conn sMap "http://cloud.aacs-us.com" wait)
              res <- takeMVar wait
              return ()
              where impulseStateMap state key = M.singleton key state

-- createWarp port cells keys conn sMap host wait = do
--   warp 3000 (MigrationRoutes cells (S.singleton . buildTestImpulseKey $ dKey) conn sMap "http://cloud.aacs-us.com" wait)

-- notWarpWarp host port migrationRoutes = do
--  finally (do
--              print )
--          (void $ do
--            print "Closing migration server"
--            let cells = (migrationRoutesAcidCell migrationRoutes)
--            void $ do
--              archiveAndHandleTVSimpleImpulseTypeStoreAC cells
--              createCheckpointAndCloseTVSimpleImpulseTypeStoreAC cells

--            app <- toWaiApp migrationRoutes
--            run 3000 app

--            )
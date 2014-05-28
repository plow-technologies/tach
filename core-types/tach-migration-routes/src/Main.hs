{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
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
import Control.Concurrent.STM.TVar
import Tach.Migration.Foundation
import Tach.Migration.Routes.Types

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
              let dKey = buildIncomingKey (KeyPid 9) (KeySource "www.aacs-us.com") (KeyDestination "http://cloud.aacs-us.com") (KeyTime 0)
                  stateName = C.unpack . DK.parseFilename . DK.encodeKey $ dKey
              impulseState <- openLocalStateFrom stateName emptyStore
              mMap <- newTVarIO (impulseStateMap impulseState dKey)
              sMap <- newTVarIO (M.singleton dKey Idle)
              warp 3000 (MigrationRoutes "./teststate/" mMap (S.singleton . buildTestImpulseKey $ (DK.DKeyRaw (KeyPid 0) (KeySource "www.aacs-us.com") (KeyDestination "http://cloud.aacs-us.com") (KeyTime 0))) conn sMap "http://cloud.aacs-us.com")
              where impulseStateMap state key = M.singleton key state

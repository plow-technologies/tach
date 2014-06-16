{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Tach.Migration.Routes.Types where

--General Haskell imports
import Data.Aeson 
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Network.AWS.S3SimpleTypes as S3
import Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
-- Acid and file related
import Data.Acid

-- Containers
import qualified Data.Set as S
import qualified Data.Map as M

--External Tach imports
import Tach.Impulse.Types.TimeValue
import Tach.Impulse.Types.Impulse
import Tach.Migration.Acidic.Types

-- Yesod and web related
import Yesod

-- Used for serializing and deserializing keys for indexing
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK

--Directly related Tach imports

--Wavelets and Compression
import Tach.Migration.Types

import Data.Acid.Cell

data MigrationRoutes = MigrationRoutes {
 migrationRoutesAcidCell :: MigrationCell
 ,migrationRoutesTVKeySet :: S.Set TVKey                             --A set of TVKeys to handle which PIDs it is responsible for
 ,s3Conn :: !S3.S3Connection
 ,stateMap :: TMVar (M.Map IncomingKey StateStatus)
 ,migrationRoutesDestination :: !String
 ,migrationRoutesWait :: MVar Int
 ,migrationRoutesS3Bucket :: String
 ,migrationRoutesStateFP :: T.Text
 ,migrationRoutesGCState :: TVar GCState
}

data GCState = GCStart | GCRunning | GCIdle deriving (Read, Show, Eq, Generic)
instance ToJSON GCState where

toMigrationTransport :: BS.ByteString
                              -> BS.ByteString
                              -> Integer
                              -> (Int, [a])
                              -> MigrationTransport Text a
toMigrationTransport src dest time (p,tvnkList) = MigrationTransport (TE.decodeUtf8 $ DK.encodeKey (DK.DKeyRaw (KeyPid p) (KeySource src) (KeyDestination dest) (KeyTime time))) tvnkList


data StateStatus = Uploading | Idle

type MigrationCell = AcidCell KeyPid KeySource KeyDestination KeyTime TVSimpleImpulseTypeStore (AcidState CellKeyStore)

buildKeyImpulseStore :: (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime) -> TVSimpleImpulseTypeStore
buildKeyImpulseStore key = TVSimpleImpulseTypeStore (ImpulseSeries (ImpulseKey key) ( IPeriodConst 0 ) (ImpulseStart 0) (ImpulseEnd 0) (ImpulseRep S.empty))

mkYesodData "MigrationRoutes" $(parseRoutesFile "migration-routes")
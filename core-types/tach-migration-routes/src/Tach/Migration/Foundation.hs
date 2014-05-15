{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Tach.Migration.Foundation where

--General Haskell imports
import Control.Concurrent.STM.TVar
import GHC.Generics
import qualified Data.ByteString as BS

-- Acid and file related
import Data.Acid

-- Containers
import qualified Data.Set as S
import qualified Data.Map as M

--External Tach imports
import Tach.Impulse.Types.TimeValue
import Tach.Migration.Acidic.Types
import Tach.Periodic
import Tach.Transformable.Types

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

--Wavelets and Compression
import qualified Codec.Compression.GZip as GZ
import Data.Wavelets.Construction


-- Data for dealing with incoming requests
newtype KeyPid = KeyPid { unKeyPid :: Int } deriving (Eq, Ord, Show,S.Serialize, Generic)
newtype KeySource = KeySource { unKeySource :: BS.ByteString } deriving (Eq, Ord, Show,S.Serialize, Generic)
newtype KeyDestination = KeyDestination { unKeyDestination :: BS.ByteString } deriving (Eq, Ord, Show, S.Serialize, Generic)
newtype KeyTime = KeyTime { unKeyTime :: Integer } deriving (Eq, Ord, Show, S.Serialize, Generic)

type IncomingKey = DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime


data MigrationRoutes = MigrationRoutes {
  migrationRoutesAcidPath :: FilePath
 ,migrationRoutesAcidMap :: TVar (M.Map IncomingKey (AcidState TVSimpleImpulseTypeStore)) --Possibly an acid map of acid states
 ,migrationRoutesTVKeySet :: S.Set TVKey                             --A set of TVKeys to handle which PIDs it is responsible for
}

mkYesodData "MigrationRoutes" $(parseRoutesFile "migration-routes")


{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving,
             ViewPatterns #-}

module Tach.Migration.Routes.Types (
    -- * Migration routes
    MigrationRoutes (..)
  , Route (..)
  , resourcesMigrationRoutes
    -- * Other types
  , Handler
  , GCState (..)
  , MigrationCell
  , StateStatus (..)
    ) where

-- General Haskell imports
import Data.Aeson 
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import GHC.Generics
import qualified Network.AWS.S3SimpleTypes as S3
import qualified Data.Text as T
-- Acid and file related
import Data.Acid

-- Containers
import qualified Data.Set as S
import qualified Data.Map as M

--External Tach imports
import Tach.Impulse.Types.TimeValue
import Tach.Migration.Acidic.Types

-- Yesod and web related
import Yesod

--Directly related Tach imports
import Tach.Migration.Types

import Data.Acid.Cell

data MigrationRoutes = MigrationRoutes {
    migrationRoutesAcidCell :: MigrationCell
  , migrationRoutesTVKeySet :: S.Set TVKey -- A set of TVKeys to handle which PIDs it is responsible for (..?)
  , s3Conn :: !S3.S3Connection
  , stateMap :: TMVar (M.Map IncomingKey StateStatus)
  , migrationRoutesDestination :: !String
  , migrationRoutesWait :: MVar Int
  , migrationRoutesS3Bucket :: String
  , migrationRoutesStateFP :: T.Text
  , migrationRoutesGCState :: TVar GCState
    }

data GCState = GCStart | GCRunning | GCIdle deriving (Read, Show, Eq, Generic)

instance ToJSON GCState where

data StateStatus = Uploading | Idle

type MigrationCell = AcidCell KeyPid KeySource KeyDestination KeyTime TVSimpleImpulseTypeStore (AcidState CellKeyStore)

mkYesodData "MigrationRoutes" $(parseRoutesFile "migration-routes")

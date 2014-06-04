{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Tach.Migration.Types where

import Tach.Migration.Types.Internal

import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK

import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import GHC.Generics
import Data.Aeson
import Data.Text

-- Data for dealing with incoming requests
newtype KeyPid = KeyPid { unKeyPid :: Int } deriving (Eq, Ord, Show,S.Serialize, Generic)
newtype KeySource = KeySource { unKeySource :: BS.ByteString } deriving (Eq, Ord, Show,S.Serialize, Generic)
newtype KeyDestination = KeyDestination { unKeyDestination :: BS.ByteString } deriving (Eq, Ord, Show, S.Serialize, Generic)
newtype KeyTime = KeyTime { unKeyTime :: Integer } deriving (Eq, Ord, Show, S.Serialize, Generic)

type IncomingKey = DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime

data MigrationTransport k tvnk = MigrationTransport {
    key :: k
  , tvNkList :: [tvnk]
} deriving (Show, Eq, Generic)

instance (ToJSON k, ToJSON tvnk) => ToJSON (MigrationTransport k tvnk) where
instance (FromJSON k, FromJSON tvnk) => FromJSON (MigrationTransport k tvnk) where
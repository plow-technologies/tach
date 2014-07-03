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
import Tach.Impulse.Types.TimeValue

type IncomingKey = DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime

data MigrationTransport k tvnk = MigrationTransport {
    key :: k
  , tvNkList :: [tvnk]
} deriving (Show, Eq, Generic)

instance (ToJSON k, ToJSON tvnk) => ToJSON (MigrationTransport k tvnk) where
instance (FromJSON k, FromJSON tvnk) => FromJSON (MigrationTransport k tvnk) where
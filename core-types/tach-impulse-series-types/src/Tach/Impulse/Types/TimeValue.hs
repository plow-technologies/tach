{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
   TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Tach.Impulse.Types.TimeValue where 

import GHC.Generics
import Data.Typeable
import Tach.Impulse.Types.Impulse 
import Data.Vector
import qualified DirectedKeys.Types as DK
import qualified Data.Serialize as S
import qualified Data.ByteString as BS

-- Data for dealing with incoming requests
newtype KeyPid = KeyPid { unKeyPid :: Int } deriving (Eq, Ord, Show,S.Serialize, Generic)
newtype KeySource = KeySource { unKeySource :: BS.ByteString } deriving (Eq, Ord, Show,S.Serialize, Generic)
newtype KeyDestination = KeyDestination { unKeyDestination :: BS.ByteString } deriving (Eq, Ord, Show, S.Serialize, Generic)
newtype KeyTime = KeyTime { unKeyTime :: Integer } deriving (Eq, Ord, Show, S.Serialize, Generic)


type TVKey = ImpulseKey (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
type TVTypeOfTime = Int
type TVParameterType = Vector Double
type TVPeriod  = ImpulsePeriod TVParameterType TVTypeOfTime 

data TVSimple = TVSimple{
  tvSimpleTime  :: Int
 ,tvSimpleId    :: Int 
 ,tvSimpleValue :: Double
} deriving (Show,Generic,Typeable,Ord,Eq)


data TVNoKey = TVNoKey {
  tvNkSimpleTime  :: Int
 ,tvNkSimpleValue :: Double
} deriving (Show,Generic,Typeable,Ord,Eq)

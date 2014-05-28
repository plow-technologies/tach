{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Tach.Migration.Foundation where

--General Haskell imports
import Control.Concurrent.STM.TVar
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Network.AWS.S3Simple as S3
import Data.Text
import qualified Data.Text.Encoding as TE
import Data.Serialize
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )

-- Acid and file related
import Data.Acid

-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

--External Tach imports
import Tach.Impulse.Types.TimeValue
import Tach.Impulse.Types.Impulse
import Tach.Migration.Acidic.Types
import Tach.Periodic
import Tach.Transformable.Types

-- Yesod and web related
import Yesod hiding (unKey)
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
import Tach.Migration.Types

import Data.Acid.Cell
import Tach.Migration.Routes.Acid
import Tach.Migration.Routes.Types

getKeyFcn :: TVSimpleImpulseTypeStore
                   -> DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
getKeyFcn = unKey . impulseSeriesKey . unTimeValueStore

defK ::  DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
defK  = (DK.DKeyRaw { getSimpleKey = KeyPid 0
                        , getSource = KeySource $ "onping.aacs-us.com"
                        , getDest = KeyDestination "onping.aacs-us.com"                        
                        , getDateTime = KeyTime 1398048132 })

migrationRoutesStoreCellKey :: CellKey KeyPid KeySource KeyDestination KeyTime TVSimpleImpulseTypeStore
migrationRoutesStoreCellKey = CellKey { getKey = getKeyFcn 
                                  , codeCellKeyFilename = fullEncode
                                  , decodeCellKeyFilename = fullDecode}

fullEncode ::  DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime -> Text
fullEncode = encodeDirectedKeyRaw

fullDecode :: Text -> Either Text (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
fullDecode = decodeDirectedKeyRaw

encodeDirectedKeyRaw :: (Serialize key, Serialize source, Serialize destination, Serialize datetime) => DK.DirectedKeyRaw key source destination datetime -> Text           
encodeDirectedKeyRaw akey = TE.decodeUtf8 $ DK.encodeKey $ akey

decodeDirectedKeyRaw :: Text -> Either Text (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
decodeDirectedKeyRaw akey =  case (DK.decodeKey $ TE.encodeUtf8 $ akey) of
                       Left e -> Left $ pack e
                       Right r -> Right r


initTVSimpleMigrationRoutes :: TVSimpleImpulseTypeStore
initTVSimpleMigrationRoutes = TVSimpleImpulseTypeStore (ImpulseSeries (ImpulseKey (DK.DKeyRaw (KeyPid 0) (KeySource "") (KeyDestination "") (KeyTime 0))) (IPeriodParameterized (V.empty)) (ImpulseStart 0) (ImpulseEnd 0) (ImpulseRep S.empty))

initSimpleMigrationRoutesStore = TVSimpleMigrationRoutes defK initTVSimpleMigrationRoutes



$(makeAcidCell 'migrationRoutesStoreCellKey 'initTVSimpleMigrationRoutes  ''TVSimpleMigrationRoutes)
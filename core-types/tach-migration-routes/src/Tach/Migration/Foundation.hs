{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Tach.Migration.Foundation where

--General Haskell imports
import Data.Text
import qualified Data.Text.Encoding as TE
import Data.Serialize
import Data.SafeCopy        ( SafeCopy )

-- Acid and file related
import Data.Acid

-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

--External Tach imports
import Tach.Impulse.Types.Impulse
import Tach.Migration.Acidic.Types
import Tach.Acid.Impulse.Cruds()

-- Yesod and web related

-- Used for serializing and deserializing keys for indexing
import qualified DirectedKeys as DK
import qualified DirectedKeys.Types as DK

--Wavelets and Compression
import Tach.Impulse.Types.TimeValue

import Data.Acid.Cell
import Filesystem.Path
import Data.Typeable

getKeyFcn :: TVSimpleImpulseTypeStore
                   -> DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
getKeyFcn = unKey . impulseSeriesKey . unTimeValueStore

defK ::  DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime
defK  = (DK.DKeyRaw { getSimpleKey = KeyPid 300
                        , getSource = KeySource $ "www.aacs-us.com"
                        , getDest = KeyDestination "http://cloud.aacs-us.com"                        
                        , getDateTime = KeyTime 0 })-- 1398048132 })

tvSimpleStoreCellKey :: CellKey KeyPid KeySource KeyDestination KeyTime TVSimpleImpulseTypeStore
tvSimpleStoreCellKey = CellKey { getKey = getKeyFcn 
                                  , codeCellKeyFilename = fullEncode
                                  , decodeCellKeyFilename = fullDecode}

fullEncode ::  DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime -> Text
fullEncode = encodeDirectedKeyRaw

fullDecode :: Text -> Either Text (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
fullDecode = decodeDirectedKeyRaw

encodeDirectedKeyRaw :: (Serialize key, Serialize source, Serialize destination, Serialize datetime) => DK.DirectedKeyRaw key source destination datetime -> Text           
encodeDirectedKeyRaw = TE.decodeUtf8 . DK.encodeKey

decodeDirectedKeyRaw :: Text -> Either Text (DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime)
decodeDirectedKeyRaw akey =  case (DK.decodeKey $ TE.encodeUtf8 $ akey) of
                       Left e -> Left $ pack e
                       Right r -> Right r


initTVSimpleStore :: TVSimpleImpulseTypeStore
initTVSimpleStore = createTVSimpleStoreFromKey defK

createTVSimpleStoreFromKey :: DK.DirectedKeyRaw KeyPid KeySource KeyDestination KeyTime -> TVSimpleImpulseTypeStore
createTVSimpleStoreFromKey key = TVSimpleImpulseTypeStore (ImpulseSeries (ImpulseKey key) (IPeriodParameterized (V.empty)) (ImpulseStart 0) (ImpulseEnd 0) (ImpulseRep S.empty))

$(makeAcidCell 'tvSimpleStoreCellKey 'initTVSimpleStore  ''TVSimpleImpulseTypeStore)

archiveAndHandleTVSimpleImpulseTypeStoreAC :: AcidCell
                                                KeyPid
                                                KeySource
                                                KeyDestination
                                                KeyTime
                                                st1
                                                (AcidState st2)
                                              -> (Filesystem.Path.FilePath
                                                  -> AcidState st1 -> IO (AcidState st1))
                                              -> IO
                                                   (M.Map
                                                      (DK.DirectedKeyRaw
                                                         KeyPid
                                                         KeySource
                                                         KeyDestination
                                                         KeyTime)
                                                      (AcidState st1))
createCheckpointAndCloseTVSimpleImpulseTypeStoreAC :: (SafeCopy st1,
                                                       Data.Typeable.Typeable st1) =>
                                                      AcidCell t1 t2 t3 t4 st (AcidState st1) -> IO ()

traverseWithKeyTVSimpleImpulseTypeStoreAC :: AcidCell t t1 t2 t3 t4 t5
                                             -> (CellKey
                                                   KeyPid
                                                   KeySource
                                                   KeyDestination
                                                   KeyTime
                                                   TVSimpleImpulseTypeStore
                                                 -> DK.DirectedKeyRaw t t1 t2 t3
                                                 -> AcidState t4
                                                 -> IO b)
                                             -> IO (M.Map (DK.DirectedKeyRaw t t1 t2 t3) b)

foldlWithKeyTVSimpleImpulseTypeStoreAC :: AcidCell t t1 t2 t3 t4 t5
                                          -> (CellKey
                                                KeyPid
                                                KeySource
                                                KeyDestination
                                                KeyTime
                                                TVSimpleImpulseTypeStore
                                              -> DK.DirectedKeyRaw t t1 t2 t3
                                              -> AcidState t4
                                              -> IO b
                                              -> IO b)
                                          -> IO b
                                          -> IO b
getTVSimpleImpulseTypeStoreAC :: AcidCell KeyPid KeySource KeyDestination KeyTime t t1
                                 -> TVSimpleImpulseTypeStore -> IO (Maybe (AcidState t))
deleteTVSimpleImpulseTypeStoreAC :: AcidCell
                                      KeyPid
                                      KeySource
                                      KeyDestination
                                      KeyTime
                                      t
                                      (AcidState (EventState DeleteAcidCellPathFileKey))
                                    -> TVSimpleImpulseTypeStore -> IO ()
insertTVSimpleImpulseTypeStoreAC :: AcidCell
                                      KeyPid
                                      KeySource
                                      KeyDestination
                                      KeyTime
                                      TVSimpleImpulseTypeStore
                                      (AcidState (EventState InsertAcidCellPathFileKey))
                                    -> TVSimpleImpulseTypeStore
                                    -> IO (AcidState TVSimpleImpulseTypeStore)
initializeTVSimpleImpulseTypeStoreAC :: Text
                                        -> IO
                                             (AcidCell
                                                KeyPid
                                                KeySource
                                                KeyDestination
                                                KeyTime
                                                TVSimpleImpulseTypeStore
                                                (AcidState CellKeyStore))
updateTVSimpleImpulseTypeStoreAC :: AcidCell KeyPid KeySource KeyDestination KeyTime t5 t6
                                          -> AcidState t5 -> TVSimpleImpulseTypeStore -> IO ()

traverseWithKeyTVSimpleImpulseTypeStoreAC = undefined

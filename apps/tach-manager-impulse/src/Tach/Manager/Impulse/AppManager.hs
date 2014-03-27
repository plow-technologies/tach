{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards #-}
module Tach.Manager.Impulse.AppManager where
import CorePrelude
import Filesystem
import Data.Default
import Keter.Types
import Keter.Main
import Data.Yaml
import Data.Yaml.FilePath
import Control.Monad 
import Keter.App
import Data.Aeson


import           Data.Conduit.Process.Unix 
import           Filesystem                (createTree, isFile, rename)
import           Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)

-- import        Control.Concurrent.Chan
-- import        Control.Concurrent
-- import        Control.Concurrent.MVar
-- import        Data.IORef 


import qualified Keter.PortPool            as PortPool

import qualified Data.Set                  as S
import qualified Data.Vector               as V
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Keter.HostManager         as HostMan
import qualified Data.HashMap.Strict as H 
import qualified Data.Map as Map
    
emptyTempFolder :: IO TempFolder.TempFolder 
emptyTempFolder = TempFolder.setup empty 

emptyPortPool :: PortSettings 
emptyPortPool = PortSettings [] 

emptyPlugins :: [Plugin]
emptyPlugins = [] 


defaultPort :: NonEmptyVector ListeningPort
defaultPort =  NonEmptyVector (LPInsecure "*" 3000) V.empty

emptyKeterConfig :: KeterConfig
emptyKeterConfig =  KeterConfig
        { kconfigDir = "./keterTest"
        , kconfigPortPool = emptyPortPool
        , kconfigListeners = defaultPort
        , kconfigSetuid = Nothing
        , kconfigBuiltinStanzas = V.empty
        , kconfigIpFromHeader = False
        }


simpleManager :: IO AppStartConfig
simpleManager = do 
  processTracker <- initProcessTracker
  tf <-  emptyTempFolder 
  hostman <- HostMan.start    
  portpool <- PortPool.start emptyPortPool
  let appStartConfig = AppStartConfig
                       { ascTempFolder = tf
                       , ascSetuid = Nothing
                       , ascProcessTracker = processTracker
                       , ascHostManager = hostman
                       , ascPortPool = portpool
                       , ascPlugins = emptyPlugins
                       , ascLog = (\_ -> return () ) 
                       , ascKeterConfig = emptyKeterConfig
                       }
  return appStartConfig



sampleBundleConfig = BundleConfig (V.fromList [defStanza]) H.empty

defFP :: FilePath
defFP = ""<.> "" </>""
      
-- defAppConfig fp = AppConfig { configExec = fp                              -- :: F.FilePath
--                             , configArgs = []                               -- :: [Text]
--                             , configHost = ""                                -- :: Text
--                             , configSsl  = False                               -- :: Bool
--                             , configExtraHosts  = S.empty                        -- :: Set Text
--                             , configRaw =  (H.empty)                                -- :: Object
--                             }

defStanza :: Stanza port
defStanza = StanzaBackground (defBackgroundConfig defFP)


defBackgroundConfig :: FilePath -> BackgroundConfig
defBackgroundConfig fp = BackgroundConfig
                      { bgconfigExec = fp
                      , bgconfigArgs = V.empty
                      , bgconfigEnvironment = Map.empty
                      , bgconfigRestartCount = LimitedRestarts 2
                      , bgconfigRestartDelaySeconds = 10
                      }


{-|

data App = App
    { appModTime        :: !(TVar (Maybe EpochTime))
    , appRunningWebApps :: !(TVar [RunningWebApp])
    , appBackgroundApps :: !(TVar [RunningBackgroundApp])
    , appId             :: !AppId
    , appHosts          :: !(TVar (Set Host))
    , appDir            :: !(TVar (Maybe FilePath))
    , appAsc            :: !AppStartConfig
    , appRlog           :: !(TVar (Maybe RotatingLog))
    }

|-}


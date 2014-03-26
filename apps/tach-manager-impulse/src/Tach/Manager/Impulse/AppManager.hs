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
import           Data.Conduit.Process.Unix 
import           Filesystem                (createTree, isFile, rename)
import           Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>),empty)
import           Control.Concurrent.Chan
import           Control.Concurrent
import           Control.Concurrent.MVar
import Data.IORef 
import qualified Keter.PortPool            as PortPool

import qualified Control.Monad.Trans.State as S
import qualified Data.HashMap.Strict       as HMap
import qualified Data.Map as Map
import qualified System.Random             as R
import qualified Data.Vector               as V
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Keter.HostManager         as HostMan

    
emptyTempFolder :: IO TempFolder.TempFolder 
emptyTempFolder = TempFolder.setup empty 

emptyPortPool :: PortSettings 
emptyPortPool = PortSettings [] 

emptyPlugins :: [Plugin]
emptyPlugins = [] 

defaultPort =  NonEmptyVector (LPInsecure "*" 80) V.empty

emptyKeterConfig =  KeterConfig
        { kconfigDir = "."
        , kconfigPortPool = emptyPortPool
        , kconfigListeners = defaultPort
        , kconfigSetuid = Nothing
        , kconfigBuiltinStanzas = V.empty
        , kconfigIpFromHeader = False
        }


simpleManager fle = do 
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
  return () 

         
testMonitorProcess = do
  ptrack <- initProcessTracker
  rotLog <- openRotatingLog "temp" 10
  monitorProcess logFcn ptrack mUid exe wrkDir cmdparams envs rotLog extFcn
    where 
      logFcn = (\_ -> return () ) 
      mUid = Nothing 
      exe = "./dist/build/toyproc/toyproc" 
      wrkDir = "./"
      cmdparams = [] 
      envs = [] 
      extFcn = (\_ -> return True) 
      
      

A generic process loadeder for keter, the process manager by Michael Snoyman





\begin{code}
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards, ScopedTypeVariables  #-}
module Tach.Manager.Impulse.Plugin where 

import CorePrelude
import Filesystem
import Data.Default
import Keter.Types
import Keter.Main
import Data.Yaml
import Control.Monad 
import           Filesystem                (createTree, isFile, rename)
import           Filesystem.Path.CurrentOS (directory, encodeString, (<.>))
import           Control.Concurrent.Chan
import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Control.Monad.Trans.State as S
import qualified Data.HashMap.Strict       as HMap
import qualified Data.Map as Map
import qualified System.Random             as R
\end{code}

The Settings Template and Command Template are designed to ensure you have 
provided the minimum definition to set up a process with keter

The Working Path is the path your executable will be run in. 



\begin{code}

data SettingsTemplate a = SettingsTemplate { 
      settingsRecord :: a ,
      appNameRetrieval :: (a -> Appname) 
--      coreConfigRetrieval :: (a -> b)
                          
}


data CommandTemplate info = GetConfig Appname (info -> IO ())

type WorkingPath = FilePath

\end{code}


The Make Plugin command is what is used to wrap your process in. 


\begin{code}
makePlugin ::  FilePath -> IO Plugin
makePlugin fp = do
\end{code}
If the file isn't present, we want to create the entire directory tree for it.
\begin{code}

    createTree $ directory fp
    e <- isFile fp
    case e of
        False-> fail "Error invalid file"
        True -> go ()
        where 
          go _ = do
                chan <- newChan
                _  <- R.newStdGen
                -- FIXME stop using the worker thread approach?
                void $ forkIO $ forever $ loop chan
                return Plugin
                           { pluginGetEnv = (\appname o -> do
                                               case HMap.lookup "postgres" o of
                                                 Just (Bool True) -> do
                                                   x <- newEmptyMVar
                                                   writeChan chan $ GetConfig appname $ putMVar x
                                                   _ <- takeMVar x
                                                   return []
                                                 _ -> return [])
                           }
          loop chan = do
                GetConfig appname f <- readChan chan 
                return $ f appname


\end{code}


\begin{code}

\end{code}

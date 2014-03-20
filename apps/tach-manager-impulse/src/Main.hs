{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies          #-}

module Main where

import Data.Conduit.Process.Unix
import Control.Concurrent
import Control.Monad
import Yesod
import Yesod.Core

data ImpulseManagerRoutes = ImpulseManagerRoutes{
  impulseManagerStatus :: MVar Int
}

mkYesod "ImpulseManagerRoutes" [parseRoutes|
/ HomeR GET
/new NewNodeR GET
|]

instance Yesod ImpulseManagerRoutes

main :: IO ()
main = do
  putStrLn "Starting"
  managerStatus <- newEmptyMVar
  _ <- forkIO $ startWarpNode managerStatus
  id <- readMVar managerStatus
  putStrLn "Stopping"


startWarpNode :: MVar Int -> IO ()
startWarpNode status = do
  warp 3000 (ImpulseManagerRoutes status)

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getNewNodeR :: Handler Html
getNewNodeR = do
  master <- getYesod
  liftIO $ putMVar (impulseManagerStatus master) 5
  defaultLayout [whamlet|New Node?!?!|]
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies          #-}

module Main where

import Control.Concurrent
import Control.Monad
import Yesod
import Yesod.Core


data ImpulseNodeRoutes = ImpulseNodeRoutes {
  impulseNodeThreadId :: MVar Int
}

mkYesod "ImpulseNodeRoutes" [parseRoutes|
/ HomeR GET
/kill KillNodeR GET
|]

instance Yesod ImpulseNodeRoutes



main :: IO ()
main = do
  putStrLn "Starting"
  tempId <- newEmptyMVar
  threadId <- forkIO $ startWarpNode tempId
  tempoIdt <- takeMVar tempId
  putStrLn "Killed"
startWarpNode :: MVar Int -> IO ()
startWarpNode tId = do
  warp 3000 (ImpulseNodeRoutes tId)

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getKillNodeR :: Handler Html
getKillNodeR = do
  master <- getYesod
  liftIO $ putMVar (impulseNodeThreadId master) 1
  defaultLayout [whamlet|Killing?!?!|]
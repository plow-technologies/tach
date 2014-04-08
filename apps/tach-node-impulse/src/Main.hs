{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings  #-}

module Main where

import System.Environment
import Tach.Migration.Routes 
import Control.Concurrent
import Control.Monad
import Data.Acid
import Data.Acid.Advanced
import System.Console.CmdArgs
import Yesod
import Yesod.Core
import Args

data ImpulseNodeRoutes = ImpulseNodeRoutes {
  impulseNodeThreadId :: MVar Int
}

-- mkYesod "ImpulseNodeRoutes" [parseRoutes|
-- / HomeR GET
-- /kill KillNodeR GET
-- |]

--  instance Yesod ImpulseNodeRoutes



main :: IO ()
main = do  
  putStrLn "Starting"
  tempId <- newEmptyMVar
  void $ startWarpNode tempId
--  tempoIdt <- takeMVar tempId
  putStrLn "Killed"






-- | NEEd to integrate these with Migration routes
startWarpNode :: MVar Int -> IO ()
startWarpNode tId = do
  arg <- cmdArgs impulseArgs
--   portS <- getEnv "PORT"  >>= return.read :: (IO Int  )
  impulseState <- openLocalStateFrom "teststate" (emptyStore)
  warp (port arg) (MigrationRoutes "./teststate/" impulseState (buildTestImpulseKey 0))

  -- warp 3000 (MigrationRoutes tId)


-- getKillNodeR :: Handler Html
-- getKillNodeR = do
--   master <- getYesod
--   liftIO $ putMVar (impulseNodeThreadId master) 1
--   defaultLayout [whamlet|Killing?!?!|]

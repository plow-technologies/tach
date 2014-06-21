{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Streaming.Network.Internal

import Yesod.Core
import Control.Monad
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Persist.Mongo.Settings
import Tach.Query.Foundation
import Tach.Query.Routes
import Tach.Query.Types

main :: IO ()
main = do
  mongoConf <- readConf "mongoDBConfig.yml"
  startQueryServer (QueryYesod mongoConf)
  putStrLn "Test"


readConf :: FilePath -> IO MongoDBConf
readConf fp = do
  cont <- readDBConf fp
  case cont of
    Left _ -> fail "Error reading mongo config file"
    Right conf -> return conf

startQueryServer :: YesodDispatch site => site -> IO ()
startQueryServer app = do
  putStrLn ("Starting query server" :: String)
  app <- toWaiApp app
  void $ startServer app 
  return ()

startServer app = do
  let warpSettings = (setFdCacheDuration 0) . (setTimeout 120) . (setHost (Host $ T.unpack "127.0.0.1")) . (setPort 2121) $ defaultSettings
  runSettings warpSettings app
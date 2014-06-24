{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleContexts, BangPatterns,DeriveGeneric #-}

module Tach.Query.Routes where


import           Data.Aeson
import           Tach.Query.Foundation
import           Tach.Query.Types
import           Yesod hiding (runDB)
import Persist.Mongo.Settings
import Control.Applicative
import qualified Data.Either as E
import qualified Database.Persist.MongoDB as PM
import qualified Database.MongoDB as M
import Safe
import Data.Text.Read (decimal)
import Data.Text (unpack)
import Data.Time
import Tach.Query.Foundation
import Network.HTTP.Types


mkYesodDispatch "QueryYesod" resourcesQueryYesod
instance Yesod QueryYesod

postParameterListerR :: Handler Value
postParameterListerR = do
  master <- getYesod
  eQuery <- parseJsonBody :: Handler (Result ParameterQuery)
  case eQuery of
    (Error _) -> do
      sendResponseStatus status501 (toJSON parseError)
    (Success query) -> do
      othList <- getHistoryParameterList (mongoConf master) query
      return . toJSON $ othList
  where parseError :: String
        parseError = "Error parsing query"


getHistoryParameterList :: MongoDBConf -> ParameterQuery -> Handler [OnpingTagHistory]
getHistoryParameterList dbConf phco = do 
  rawBsonParameters <- runDBConf dbConf $ runStagedQuery getAllRaw (makeOpListRM phco)
  let parameters :: [OnpingTagHistory]
      parameters = entityVal <$> (E.rights $ PM.docToEntityEither <$> rawBsonParameters)
      etpid = decimal.phistoryPID $ phco
  case etpid of
    Left _ -> sendResponseStatus status501 (toJSON errMessage) 
    Right (pid, _) -> return parameters
  where runStagedQuery :: (MonadIO m, MonadBaseControl IO m) => (M.Selector -> PM.Action m [M.Document]) -> [M.Document] -> PM.Action m [M.Document] 
        runStagedQuery _ [] = return []
        runStagedQuery f !qList = do
          let mx = 50
          lst <- f ["$or" M.=: (take mx qList)]
          lst2 <- runStagedQuery f (drop mx qList)   
          return $ lst ++ lst2 
        errMessage :: String
        errMessage = "Error querying DB"

getAllRaw :: (MonadIO m, MonadBaseControl IO m) => M.Selector -> PM.Action m [M.Document]
getAllRaw q = M.rest =<< M.find (M.select q "onping_tag_history")

makeOpListRM :: ParameterQuery -> [M.Document]
makeOpListRM phco =  let pid :: Maybe Int
                         pid   = readMay.unpack.phistoryPID $ phco
                         start = phistoryStart phco 
                         end   = phistoryEnd phco 
                         step  = realToFrac.phistoryStep  $ phco 
                         delta = realToFrac.phistoryDelta $ phco 
                         mx   = 100000
                         fList = (\lst -> ("pid" M.=: pid):lst).(makeDeltaRM delta) <$> (makeStraightTimeList start end step)
                     in  (take mx fList)

makeDeltaRM :: NominalDiffTime ->UTCTime -> [M.Field]
makeDeltaRM delta time = ["time" M.=: ["$gt" M.=: time, "$lte" M.=: dt]]
    where dt :: UTCTime
          dt = (addUTCTime delta time)

makeStraightTimeList ::  UTCTime -> UTCTime -> NominalDiffTime -> [UTCTime]
makeStraightTimeList start stop stp = take amount (iterate applyStep start )
    where applyStep :: UTCTime -> UTCTime 
          applyStep = addUTCTime stp 
          fAmount :: NominalDiffTime
          fAmount = (diffUTCTime stop start) /stp
          amount  :: Int
          amount  = floor fAmount
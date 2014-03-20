{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}


module Tach.Acid.Impulse.Cruds.Create where
-- Generic Haskell Stuff 

import Control.Applicative
import qualified Data.Traversable as T
import Filesystem.Path
import CorePrelude
import Data.Aeson
-- Containers 
import Data.Sequence
import Data.IntMap
import Data.Vector
-- import Data.ByteString

-- ACID Specific
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Typeable (Typeable)

-- Impulse Specific 
import Tach.Acid.Impulse.State 

import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValueSeries (TVSStart,TVSEnd)
import Tach.Migration.Acidic.Types
import Tach.Migration.Acidic.Instances 




-- |Create an ImpulseTypeStore and return the filepath it was created

-- createTVSimpleImpulseTypeStore :: (ImpulseKey Integer) (ImpulsePeriod (Vector Double) (Integer)) (ImpulseStart Integer) (ImpulseEnd Integer) (ImpulseRep (Seq TVNoKey))
createTVSimpleImpulseTypeStore :: FilePath -> TVKey -> TVPeriod -> TVSStart -> TVSEnd -> (ImpulseRep (Seq TVNoKey)) -> IO (Either Text FilePath)
createTVSimpleImpulseTypeStore = undefined




-- |ACID Functions

newtype SuccessValue = SuccessValue { getSuccess :: Value}
newtype ErrorValue = ErrorValue { getError :: Value } 


-- | Insert one element into an ImpulseTypeStore , return the number now in the store
type SequenceCount = Int

insertTVSimpleImpulse :: TVKey -> (TVNoKey) -> Update TVSimpleImpulseTypeStore (Either ErrorValue SuccessValue)
insertTVSimpleImpulse tk d = do
  st@(TVSimpleImpulseTypeStore (ImpulseSeries {impulseSeriesKey = k, impulseSeriesRep = r})) <- get 
  case st of 
    _ 
      | k == tk -> (return . Left $ ErrorValue $  object ["iencorrectKey" .= (unKey tk) , "correctKey" .= (unKey k)]  )
      | otherwise -> return . Left $ ErrorValue $  object ["iencorrectKey" .= (unKey tk) , "correctKey" .= (unKey k)]  
     where
      insertFcn = undefined
      insertVal = (insertFcn d ) <$> r



-- | Like above but batch insert
insertManyTVSimpleImpulse :: TVKey ->  (Seq TVNoKey) -> IO SequenceCount 
insertManyTVSimpleImpulse = undefined 






{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Tach.Acid.Impulse.Cruds where


-- Generic Haskell Stuff 
import Control.Applicative
import qualified Filesystem.Path.CurrentOS as P
import Filesystem.Path
import CorePrelude
import Data.Aeson

-- Containers 
import Data.Sequence
import Data.IntMap
import Data.Vector (Vector)
import qualified Data.Text as T

-- ACID Specific
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState , createCheckpoint )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Typeable (Typeable)

-- Impulse Specific 
import Tach.Acid.Impulse.Cruds.Create
import Tach.Acid.Impulse.Cruds.Read
import Tach.Acid.Impulse.Cruds.Delete
import Tach.Impulse.Types.TimeValue 
import Tach.Impulse.Types.Impulse
import Tach.Impulse.Types.TimeValueSeries (TVSStart,TVSEnd)
import Tach.Migration.Acidic.Types
import Tach.Migration.Acidic.Instances

-- import Tach.Migration.Acidic.Instances 

-- |Create TVSimple ImpulseTypeStore is called the first time a vector series is made 

createTVSimpleImpulseTypeStore :: FilePath  -> TVSimpleImpulseTypeStore -> IO () -- (Either ErrorValue SuccessValue) 
createTVSimpleImpulseTypeStore fp tvsIStore= do 
  newImpulseStore <- openLocalStateFrom decodedFilePath tvsIStore
  createCheckpoint newImpulseStore
  closeAcidState newImpulseStore
  return () 
    where decodedFilePath = P.encodeString fp 



-- |Acid Acessors and types
$(makeAcidic ''TVSimpleImpulseTypeStore [ 'insertTVSimpleImpulse , 'insertManyTVSimpleImpulse
                                        , 'getTVSimpleImpulse    , 'getTVSimpleImpulseMany
                                        , 'deleteTVSimpleImpulse , 'deleteManyTVSimpleImpulse ])

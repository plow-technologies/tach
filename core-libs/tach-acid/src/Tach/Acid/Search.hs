{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Tach.Acid.Search where

import           Control.Applicative
import           Control.Exception                (bracket)
import           Control.Monad                    (msum)
import           Control.Monad.Reader             (ask)
import           Control.Monad.State              (get, put)
import           Data.Acid                        (AcidState, Query, Update,
                                                   makeAcidic, openLocalState)
import           Data.Acid.Advanced               (query', update')
import           Data.Acid.Local                  (createCheckpointAndClose)
import           Data.Bifunctor
import qualified Data.Foldable                    as F
import           Data.Monoid
import           Data.SafeCopy                    (base, deriveSafeCopy)
import qualified Data.Sequence                    as SEQ
import           Data.Traversable
import           Data.Typeable
import           GHC.Generics
import           Tach.Class.Queryable
import           Tach.DB.Types
import           Tach.DB.Types.Acid
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Wavelet
import           Tach.Types.Classify
import           Tach.Types.Classify.Lens

queryManyTransfomredStore :: Int -> Int -> Int -> Query TransformedStore (SEQ.Seq TVNoKey)
queryManyTransfomredStore step start end = do
  classified <- ask
  return $ queryTf classified step start end
  where
      queryTf :: TransformedStore -> Int -> Int -> Int -> SEQ.Seq TVNoKey
      queryTf stored sp st en =
        let raw = transformedData stored
        in query sp st en raw

queryOneTransformedStore :: Int -> Query TransformedStore TVNoKey
queryOneTransformedStore = undefined
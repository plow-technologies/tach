{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Tach.DB.Types.Acid where

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
import           Data.Traversable
import           Data.Typeable
import           Data.Wavelets.Scaling
import           GHC.Generics
import           Tach.Class.Queryable
import           Tach.DB.Types
import           Tach.DB.Types.Internal
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse
import           Tach.Transformable.Types.Wavelet
import           Tach.Types.Classify
import           Tach.Types.Classify.Lens
import qualified Data.Sequence as SEQ
import           Tach.Transformable.Types.Wavelet.Core


type ClassifiedData = Classify ImpulseTransformed (Classify (WaveletTransformed Double) ())

data TransformedStore = TransformedStore { transformedData :: [ClassifiedData] } deriving (Eq, Ord, Show, Typeable, Generic)

$(deriveSafeCopy 0 'base ''Classify)
$(deriveSafeCopy 0 'base ''TVNoKey)
$(deriveSafeCopy 0 'base ''SeriesFactors)
$(deriveSafeCopy 0 'base ''OldSeriesFactors)
$(deriveSafeCopy 0 'base ''NewSeriesFactors)
$(deriveSafeCopy 0 'base ''ImpulseTransformed)
$(deriveSafeCopy 0 'base ''WaveletTransformed)
$(deriveSafeCopy 0 'base ''TransformedStore)
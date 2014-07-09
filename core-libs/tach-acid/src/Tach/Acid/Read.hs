{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Tach.Acid.Read where

import           Control.Monad.Reader             (ask)
import           Control.Monad.State              (get, put)
import           Data.Acid                        (AcidState, Query, Update,
                                                   makeAcidic, openLocalState)
import           Tach.DB.Types.Acid
import           Tach.Class.Bounds

queryTransformedStoreBounds :: Query TransformedStore (Int,Int)
queryTransformedStoreBounds = do
  classified <- ask
  return . bounds . transformedData $ classified

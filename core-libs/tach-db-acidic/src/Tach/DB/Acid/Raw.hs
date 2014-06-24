{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,OverloadedStrings,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude
  , TemplateHaskell, TypeFamilies, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tach.DB.Acid.Raw where

import           Data.Acid
import           Tach.DB.Acid.Raw.Types

{- The seperated modules for Tach.DB.Acid.Raw -}
import           Tach.DB.Acid.Raw.Create as Tach.DB.Acid.Raw
import           Tach.DB.Acid.Raw.Delete as Tach.DB.Acid.Raw
import           Tach.DB.Acid.Raw.Read   as Tach.DB.Acid.Raw
import           Tach.DB.Acid.Raw.Types  as Tach.DB.Acid.Raw
import           Tach.Impulse.Types.TimeValue  as Tach.DB.Acid.Raw

{-
  The raw acid state holds a size limited set of TVNoKeys
  that are held in a map of keys to sets

  The individual acid cell should have methods to insert,
  remove, and query in general ways

  This is just a nice way to organize all the imports for the raw API
-}


$(makeAcidic ''TVSimpleRawStore [ 'insertTVSimpleImpulseRaw, 'insertManyTVSimpleImpulseRaw
                                , 'getTVSimpleImpulseRaw    , 'getManyTVSimpleImpulseRaw
                                , 'deleteTVSimpleImpulseRaw , 'deleteManyTVSimpleImpulseRaw
                                , 'getTVSimpleImpulseRawSize, 'getTVSimpleImpulseRawTimeBounds ])

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tach.DB.Acid.Raw where


{-
  The raw acid state holds a size limited set of TVNoKeys
  that are held in a map of keys to sets

  The individual acid cell should have methods to insert,
  remove, and query in general ways

  This is just a nice way to organize all the imports for the raw API
-}


import           Tach.DB.Acid.Transform.Create as Tach.DB.Acid.Raw
import           Tach.DB.Acid.Transform.Delete as Tach.DB.Acid.Raw
import           Tach.DB.Acid.Transform.Read   as Tach.DB.Acid.Raw
import           Tach.DB.Acid.Transform.Types  as Tach.DB.Acid.Raw
import           Tach.Impulse.Types.TimeValue  as Tach.DB.Acid.Raw


-- $(makeAcidic ''TVSimpleImpulseTypeStore [ 'insertTVSimpleImpulse , 'insertManyTVSimpleImpulse
--                                         , 'getTVSimpleImpulse    , 'getTVSimpleImpulseMany
--                                         , 'deleteTVSimpleImpulse , 'deleteManyTVSimpleImpulse
--                                         , 'getTVSimpleImpulseSize, 'getTVSimpleImpulseTimeBounds ])
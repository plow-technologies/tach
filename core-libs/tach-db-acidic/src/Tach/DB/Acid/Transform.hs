
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tach.DB.Acid.Transform where

import qualified DirectedKeys.Types           as DK
import           Tach.Impulse.Types.TimeValue

{-
  The transform acid state holds a size limited set of transformed TVNoKeys
  that are held in a map of keys to sets

  The individual acid cell should have methods to insert,
  remove, and query in general ways
-}

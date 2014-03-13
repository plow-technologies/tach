{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Tach.Impulse.Types.Impulse where 

import GHC.Generics

import Data.Typeable


{- | 

'ImpulseSeries is the fundamental datatype that represents all the various kinds of time related event data.

It could be a-periodic sampling from a cell modem, a set of compressed files etc.. . 

The idea is any set of values represented by 'rep is expected to have period 'isp, which may be a parameterized value.  
The series start value ('st) and end value ('en) are there to be used to identify individual data points. 

|-}


data ImpulseSeries isp st en rep = ImpulseSeries{ 
      impulseSeriesPeriod :: isp, 
      impulseSeriesStart  :: st , 
      impulseSeriesEnd    :: en ,
      impulseSeriesRep    :: rep 
} deriving (Generic,Typeable)

-- | some standard newtype helpers for defining isp, rep st en


newtype ImpulseStart  a = ImpulseStart { unStart ::   a} deriving (Generic,Typeable)  
newtype ImpulseEnd    a = ImpulseEnd { unEnd ::       a} deriving (Generic,Typeable)   
newtype ImpulseRep    a = ImpulseRep { unRep ::       a} deriving (Generic,Typeable) 


instance Functor ImpulseRep  where 
    fmap f (ImpulseRep r) = ImpulseRep (f r)


instance Functor (ImpulseSeries isp st en ) where 
    fmap f (ImpulseSeries a b c r) = ImpulseSeries a b c (f r)


data ImpulsePeriod  a b = IPeriodConst b | IPeriodParameterized (a -> b)
                          deriving (Generic,Typeable) 

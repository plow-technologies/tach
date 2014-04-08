{-# LANGUAGE DeriveDataTypeable #-}

module Args where 

import System.Console.CmdArgs



data ImpulseArgs = ImpulseArgs { port:: Int } 
  deriving (Show,Data,Typeable)

impulseArgs :: ImpulseArgs 
impulseArgs = ImpulseArgs { port = 3000  &= help "Enter a valid portnumber" }

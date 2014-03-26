{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NoRecordWildCards #-}

import CorePrelude
import Control.Monad
import Control.Concurrent (threadDelay)
import           Filesystem                (createTree, isFile, rename,writeFile)
import           Filesystem.Path.CurrentOS (directory, encodeString, (<.>),(</>))



wrtieFilePath = "."</>"test"<.>"txt"


main :: IO ()
main = forever $ do 
         print "Test" 
         writeFile wrtieFilePath "Here is a test bytestring"
         threadDelay (60*100000*10)







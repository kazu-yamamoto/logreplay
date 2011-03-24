{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad
import Report
import Job
import System.Environment
import System.IO

main :: IO ()
main = do
    [inp,out,n] <- getArgs
    let numOfWorkers = read n
    hdl <- openFile inp ReadMode
    atom <- newAtom numOfWorkers (logspec out)
    forkIO $ jobFeed atom hdl numOfWorkers
    replicateM_ numOfWorkers $ forkIO (getter atom)
    forkIO $ reporter atom
    waitFor atom
 where
   logspec file = FileLogSpec {
       log_file = file
     , log_file_size = 16777216
     , log_backup_number = 10
     , log_buffer_size = 16384
     , log_flush_period = 10
     }

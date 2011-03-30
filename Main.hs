{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad
import Job
import System.Environment
import System.IO

main :: IO ()
main = do
    [n,inp] <- getArgs
    let numOfWorkers = read n
    hdl <- openFile inp ReadMode
    atom <- newAtom numOfWorkers
    forkIO $ jobFeed atom hdl numOfWorkers
    replicateM_ numOfWorkers $ forkIO (getter atom 0)
    forkIO $ reporter atom
    waitFor atom

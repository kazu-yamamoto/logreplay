{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import ApacheLog
import Network.HTTP.Enumerator
import Log

data ATOM = ATOM {
    jobQueue :: TChan (Maybe Log)
  , repQueue :: TChan BL.ByteString
  , jobsDone :: TVar Int
  , activeWorkers :: TVar Int
  }

numOfWorkers :: Int
numOfWorkers = 5

main :: IO ()
main = do
    atom <- ATOM <$> newTChanIO
                 <*> fileInit logspec
                 <*> newTVarIO 0
                 <*> newTVarIO numOfWorkers
    forkIO $ logFeed numOfWorkers atom
    replicateM_ numOfWorkers $ forkIO (getter atom)
    forkIO $ reporter atom
    waitFor atom
 where
   logspec = FileLogSpec {
       log_file = "log"
     , log_file_size = 16777216
     , log_backup_number = 10
     , log_buffer_size = 16384
     , log_flush_period = 10
     }

reporter :: ATOM -> IO ()
reporter atom = forever $ do
    threadDelay 1000000
    n <- atomically $ readTVar (jobsDone atom)
    m <- atomically $ readTVar (activeWorkers atom)
    putStrLn $ show n ++ " " ++ show m

waitFor :: ATOM -> IO ()
waitFor atom = atomically $ do
  active <- readTVar (activeWorkers atom)
  check (active == 0)

getter :: ATOM -> IO ()
getter atom = do
   mx <- atomically $ readTChan jobq
   case mx of
       Nothing -> atomically $ do
           n <- readTVar actv
           writeTVar actv (n-1)
       Just x -> if (logMethod x == "GET")
           then do
               req <- parseUrl $ "http://localhost" ++ BS.unpack (logPath x)
               httpLbs req
               atomically $ do
                   n <- readTVar done
                   writeTVar done (n+1)
                   writeTChan repq $ BL.pack $ show x ++ "\n"
               getter atom
           else getter atom
  where
    jobq = jobQueue atom
    repq = repQueue atom
    actv = activeWorkers atom
    done = jobsDone atom

logFeed :: Int -> ATOM -> IO ()
logFeed n atom = do
    ls <- bslines <$> BL.getContents
    let xs = filter isJust (map apache ls) ++ replicate n Nothing
    mapM_ write xs
  where
    jobq = jobQueue atom
    write x = atomically $ writeTChan jobq x

bslines :: BL.ByteString -> [ByteString]
bslines bs = case BL.break (== '\n') bs of
    (bs',"")   -> toStrict bs' : []
    (bs',rest) -> toStrict bs' : bslines (BL.tail rest)
  where
    toStrict = BS.concat . BL.toChunks


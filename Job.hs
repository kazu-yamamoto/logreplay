{-# LANGUAGE OverloadedStrings #-}

module Job where

import ApacheLog
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Network.HTTP.Enumerator
import Report
import System.IO

data ATOM = ATOM {
    jobQueue :: TChan (Maybe Log)
  , repQueue :: TChan BL.ByteString
  , jobsDone :: TVar Int
  , activeWorkers :: TVar Int
  }

newAtom :: Int -> FileLogSpec -> IO ATOM
newAtom n logspec =
    ATOM <$> newTChanIO
--         <*> fileInit logspec
         <*> stdoutInit
         <*> newTVarIO 0
         <*> newTVarIO n

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

jobFeed :: ATOM -> Handle -> Int-> IO ()
jobFeed atom hdl n = do
    ls <- bslines <$> BL.hGetContents hdl
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


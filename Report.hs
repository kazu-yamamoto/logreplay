{-# LANGUAGE OverloadedStrings #-}

module Report where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import Network.Wai
import Network.Wai.Application.Classic
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Locale
import System.Posix

data FileLogSpec = FileLogSpec {
    log_file :: String
  , log_file_size :: Integer
  , log_backup_number :: Int
  , log_buffer_size :: Int
  , log_flush_period :: Int
  }

fileCheck :: FileLogSpec -> IO ()
fileCheck spec = do
    dirperm <- getPermissions dir
    unless (writable dirperm) $ exit $ dir ++ " is not writable"
    fileexist <- doesFileExist file
    when fileexist $ do
        fileperm <- getPermissions file
        unless (writable fileperm) $ exit $ file ++ " is not writable"
  where
    file = log_file spec
    dir = takeDirectory file
    exit msg = hPutStrLn stderr msg >> exitFailure

fileInit :: FileLogSpec -> IO (TChan ByteString)
fileInit spec = do
    hdl <- open spec
    tvar <- newTVarIO hdl
    chan <- newTChanIO
    forkIO $ fileFlusher tvar spec
    forkIO $ fileSerializer chan tvar
    return chan

fileFlusher :: TVar Handle -> FileLogSpec -> IO ()
fileFlusher tvar spec = forever $ do
    threadDelay $ log_flush_period spec
    hdl <- readTVarIO tvar
    hFlush hdl
    size <- hFileSize hdl
    if size > log_file_size spec
       then do
        hClose hdl
        locate spec
        newhdl <- open spec
        atomically $ writeTVar tvar newhdl
       else atomically $ writeTVar tvar hdl

fileSerializer :: TChan ByteString -> TVar Handle -> IO ()
fileSerializer chan tvar = forever $ do
    xs <- atomically $ readTChan chan
    hdl <- readTVarIO tvar
    BL.hPut hdl xs
    atomically $ writeTVar tvar hdl

open :: FileLogSpec -> IO Handle
open spec = do
    hdl <- openFile file AppendMode
    setFileMode file 0o644
    hSetEncoding hdl latin1
    hSetBuffering hdl $ BlockBuffering (Just $ log_buffer_size spec)
    return hdl
  where
    file = log_file spec

locate :: FileLogSpec -> IO ()
locate spec = mapM_ move srcdsts
  where
    path = log_file spec
    n = log_backup_number spec
    dsts' = reverse . ("":) . map ('.':) . map show $ [0..n-1]
    dsts = map (path++) dsts'
    srcs = tail dsts
    srcdsts = zip srcs dsts
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst

----------------------------------------------------------------

stdoutInit :: IO (TChan ByteString)
stdoutInit = do
    chan <- newTChanIO
    forkIO $ stdoutSerializer chan
    return chan

stdoutSerializer :: TChan ByteString -> IO ()
stdoutSerializer chan = forever $ do
    xs <- atomically $ readTChan chan
    BL.putStr xs

----------------------------------------------------------------

mightyLogger :: TChan ByteString -> Request -> Status -> IO ()
mightyLogger chan req st = do
    zt <- getZonedTime
    addr <- getPeerAddr (remoteHost req)
    atomically $ writeTChan chan $ BL.fromChunks (logmsg addr zt)
  where
    logmsg addr zt = [
        BS.pack addr
      , " - - ["
      , BS.pack (formatTime defaultTimeLocale "%d/%b/%Y:%T %z" zt)
      , "] \""
      , requestMethod req
      , " "
      , pathInfo req
      , "\" "
      , BS.pack (show . statusCode $ st)
      , " - \"" -- size
      , lookupRequestField' fkReferer req
      , "\" \""
      , lookupRequestField' fkUserAgent req
      , "\"\n"
      ]

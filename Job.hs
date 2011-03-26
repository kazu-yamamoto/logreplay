{-# LANGUAGE OverloadedStrings #-}

module Job where

import ApacheLog
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as Exc
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Enumerator.List as EL
import Data.Enumerator hiding (replicate, filter, map)
import Data.Maybe
import Network.HTTP.Enumerator
import Report
import System.IO
import System.IO.Error (isEOFError)

data ATOM = ATOM {
    jobQueue :: TChan (Maybe Log)
  , repQueue :: TChan BL.ByteString
  , jobsDone :: TVar Int
  , activeWorkers :: TVar Int
  }

newAtom :: Int -> FileLogSpec -> IO ATOM
newAtom n logspec =
    ATOM <$> newTChanIO
         <*> fileInit logspec
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

jobFeed :: ATOM -> Handle -> Int -> IO ()
jobFeed atom hdl n =
    run_ $ enumList 64 (replicate n Nothing)
        $$ (enumHandle hdl `joinE` EL.map apache `joinE` EL.filter isJust)
        $$ iterFeed atom
    
iterFeed :: ATOM -> Iteratee (Maybe Log) IO ()
iterFeed atom = do
    mx <- EL.head
    case mx of
        Nothing -> return ()
        Just x  -> liftIO (write x) >> iterFeed atom
  where
    jobq = jobQueue atom
    write x = atomically $ writeTChan jobq x

enumHandle :: MonadIO m => Handle -> Enumerator ByteString m b
enumHandle h = loop where
        loop (Continue k) = do
                maybeBytes <- tryIO getBytes
                case maybeBytes of
                        Nothing -> continue k
                        Just text -> k (Chunks [text]) >>== loop
        
        loop step = returnI step
        getBytes = Exc.catch
                (Just `fmap` BS.hGetLine h)
                (\err -> if isEOFError err
                        then return Nothing
                        else Exc.throwIO err)

tryIO :: MonadIO m => IO b -> Iteratee a m b
tryIO io = Iteratee $ do
        tried <- liftIO (Exc.try io)
        return $ case tried of
                Right b -> Yield b (Chunks [])
                Left err -> Error err

infixr 0 =$

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
ee =$ ie = joinI $ ee $$ ie

{-# LANGUAGE OverloadedStrings #-}

module Job where

import ApacheLog
import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as Exc
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Enumerator hiding (replicate, filter, map)
import qualified Data.Enumerator.List as EL
import Data.Maybe
import Network.HTTP.Enumerator
import System.IO
import System.IO.Error (isEOFError)

data ATOM = ATOM {
    jobQueue :: Chan (Maybe Log)
  , jobsDone :: MVar Int
  , activeWorkers :: MVar Int
  }

newAtom :: Int -> IO ATOM
newAtom n =
    ATOM <$> newChan
         <*> newMVar 0
         <*> newMVar n

reporter :: ATOM -> IO ()
reporter atom = forever $ do
    threadDelay 1000000
    n <- readMVar (jobsDone atom)
    putStrLn $ show n

waitFor :: ATOM -> IO ()
waitFor atom = do
  active <- readMVar (activeWorkers atom)
  if active == 0
     then return ()
     else threadDelay 1000000 >> waitFor atom

getter :: ATOM -> Int -> IO ()
getter atom n = do
   mx <- readChan jobq
   case mx of
       Nothing -> modifyMVar_ actv (return . subtract 1)
       Just x -> if (logMethod x == "GET")
           then do
               req <- parseUrl $ "http://localhost" ++ BS.unpack (logPath x)
               httpLbs req
               let n' = n + 1
               when (n' `mod` 10 == 0) $
                   modifyMVar_ done (return . (+10))
               getter atom n'
           else getter atom n
  where
    jobq = jobQueue atom
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
    write x = writeChan jobq x

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

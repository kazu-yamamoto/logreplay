{-# LANGUAGE OverloadedStrings #-}

module ApacheLog where

import Control.Concurrent
import Control.Monad
import Control.Applicative ((<$),(<$>),(<*),(*>))
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data Log = Log {
    logTime :: ByteString -- UTCTime
  , logPath :: ByteString
  , logMethod :: ByteString
  } deriving (Eq,Show)

main :: IO ()
main = do
    chan <- newChan
    forkIO (bslines <$> BL.getContents >>= mapM_ (write chan . apache))
    forever $ readChan chan >>= print
  where
    write _    Nothing = return ()
    write chan (Just x) = writeChan chan x

bslines :: BL.ByteString -> [ByteString]
bslines bs = case BL.break (== '\n') bs of
    (bs',"")   -> toStrict bs' : []
    (bs',rest) -> toStrict bs' : bslines (BL.tail rest)
  where
    toStrict = BS.concat . BL.toChunks

apache :: ByteString -> Maybe Log
apache bs = case feed (parse apacheLog bs) "" of
    Done _ x -> Just x
    _        -> Nothing

apacheLog :: Parser Log
apacheLog = do
    ipaddr
    skipSpc
    ident
    skipSpc
    user
    skipSpc
    t <- date
    skipSpc
    (m,p) <- methodPath
    trailing
    return $ Log t p m
  where
    skipSpc = () <$ char ' '
    skipNotSPCs = skipWhile (/=' ')
    notSPCs = takeWhile1 (/=' ')
    ipaddr = skipNotSPCs
    ident = skipNotSPCs
    user = skipNotSPCs
    date = char '[' *> takeWhile1 (/=']') <* char ']'
    methodPath = do
        char '"'
        m <- notSPCs
        skipSpc
        p <- notSPCs
        skipWhile (/='"')
        char '"'
        return (m,p)
    trailing = takeWhile1 (/='\n') *> endOfInput

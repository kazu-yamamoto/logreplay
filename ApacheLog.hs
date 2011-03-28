{-# LANGUAGE OverloadedStrings #-}

module ApacheLog where

import Control.Applicative ((<$),(<*),(*>))
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)

data Log = Log {
    logTime :: ByteString -- UTCTime
  , logPath :: ByteString
  , logMethod :: ByteString
  } deriving (Eq,Show)

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
    trailing = many anyChar *> endOfInput

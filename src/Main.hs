module Main where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (hGetLine)
import Data.Conduit (($$), (=$=), (=$))
import Data.Functor ((<$>))
--import Data.IORef (newIORef, writeIORef)

import System.IO (stdin)

import Challenge.Conduit
import Challenge.LogEntry
import Challenge.Statistics

main :: IO ()
main = do
    result <- parseOnly schemaParser <$> hGetLine stdin
 --   canStream <- newIORef True
    case result of
      Left err -> putStrLn $ "Error creating schema. " ++ err
      Right schema -> do
        stats <- newStatsConduit schema
        showStats <- newStatsToByteStringConduit schema
        --streamingProducer canStream $$ 
        stdInProducer $$
            logEntryConduit schema =$= stats =$= showStats 
            =$ stdOutConsumer   
    return ()

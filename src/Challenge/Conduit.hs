module Challenge.Conduit where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Char8 (ByteString, hGetLine,  pack)
import Data.Conduit (Conduit, ConduitM, Consumer, Producer, await, yield)
import Data.Conduit.Attoparsec (ParseError, PositionRange, conduitParserEither)
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.IORef (IORef, readIORef)

import Challenge.LogEntry

import System.IO (stdin, stdout)

type ParseResult = Either ParseError (PositionRange, LogEntry)

stdInProducer :: Producer IO ByteString
stdInProducer = sourceHandle stdin 

streamingProducer ::IORef Bool -> ConduitM () ByteString IO ()
streamingProducer isStreaming = do
    val <- liftIO $ readIORef isStreaming 
    when val $ do
        liftIO $ putStr "\n"
        value <- liftIO $ hGetLine stdin
        liftIO $ print value 
        yield value
        streamingProducer isStreaming
    return ()

stdOutConsumer :: Consumer ByteString IO ()
stdOutConsumer = sinkHandle stdout

logEntryConduit :: Schema -> Conduit ByteString IO ParseResult 
logEntryConduit = conduitParserEither . newLogEntryParser

-- used for debugging
entryToByteStringConduit :: Conduit ParseResult IO ByteString
entryToByteStringConduit = do
    result <- await
    case result of
      Just (Left err) -> do
          liftIO . putStrLn $ "Error: " ++ show err
          entryToByteStringConduit
      Just (Right (_, a)) -> do
          yield . pack $ show a ++ "\n"
          entryToByteStringConduit 
      _ -> return ()


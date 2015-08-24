{-# LANGUAGE OverloadedStrings #-}

module Challenge.Statistics
    ( ColumnStats(..) 
    , LogEntryStats(..)
    , MutDbl 
    , MutInt
    , MutSci
    , NumberStats(..)
    , StatsConduit
    , TextStats(..)
    , newStatsConduit
    , newStatsToByteStringConduit 
    ) where

import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (StateT, get, modify, evalStateT)

import Data.Conduit (Conduit, await, yield)
import Data.IORef
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), mempty)
import Data.Scientific (Scientific, toRealFloat, formatScientific, FPFormat(Fixed))
import Data.Word8 (_lf)

import Challenge.Conduit (ParseResult)
import Challenge.LogEntry (LogEntry(..), Schema(..), Column(..), ColumnValue(..))

import qualified Data.ByteString as B

type MutInt = IORef Int64

type MutDbl = IORef Double

type MutSci = IORef Scientific

data ColumnStats = TextCol TextStats | NumberCol NumberStats

data TextStats = TextStats
    { textAverageLength :: MutDbl 
    , textLongestCount  :: MutInt
    , textLongestVal :: IORef B.ByteString
    , textNullCount :: MutInt 
    , textShortestCount :: MutInt
    , textShortestVal :: IORef B.ByteString
    }

data NumberStats = NumberStats
    { numAverage :: MutDbl
    , numMax :: MutSci
    , numMin :: MutSci
    , numNullCount :: MutInt
    }

data LogEntryStats = LogEntryStats 
    { statsColumnStats :: [ColumnStats]
    , statsEntryCount :: MutInt
    }

type StatsConduit = Conduit ParseResult IO LogEntryStats

newStatsConduit :: Schema -> IO StatsConduit 
newStatsConduit schema = do
    stats <- newLogEntryStats schema
    let trackers = map updateColumnStats (statsColumnStats stats)
    return $ statsConduit stats trackers
    where statsConduit stats trackers = do
            result <- await
            case result of
              Just (Left err) -> do
                liftIO . putStrLn $ "Stats Conduit Error: " ++ show err
                statsConduit stats trackers
                      
              Just (Right (_, LogEntry cols)) -> do
                liftIO $ modifyIORef' (statsEntryCount stats) (+1)
                liftIO $ foldl (>>) (return ()) $ zipWith id trackers cols
                yield stats
                statsConduit stats trackers

              _ -> return ()

updateColumnStats :: ColumnStats -> Column -> IO ()
updateColumnStats (TextCol stats) column = 
    case column of
      Column _ (TextVal val) -> do
        let len = B.length val
        modifyIORef'
            (textAverageLength stats)
            (\old -> calcEMA alpha old (fromIntegral len))
        textStat textLongestVal textLongestCount (>=) (>) val len 
        textStat textShortestVal textShortestCount (<=) (<) val len

      Column _ NulVal ->  modifyIORef' (textNullCount stats) (+1)

      _ -> return()
      where
          alpha = 0.02 -- EMA alpha 2/200
          textStat valFun cntFun cmp1 cmp2 valB lenB = do
            valA <- readIORef $ valFun stats

            let lenA = B.length valA  
            if lenA == 0 then do
                writeIORef (valFun stats) valB
                writeIORef (cntFun stats) 1
            else when (cmp1 lenB lenA) $
                    if cmp2 lenB lenA || valB < valA then do
                        writeIORef (valFun stats) valB
                        writeIORef (cntFun stats) 1
                    else modifyIORef' (cntFun stats) (+1)

updateColumnStats (NumberCol stats) column =
    case column of
      Column _ (NumberVal val) -> do
       -- change the sci to dbl otherwise computation becomes excessively expensive
       let valDbl = toRealFloat val 
       
       modifyIORef' (numAverage stats) (\old -> calcEMA alpha old valDbl)
       minMax numMax val (>)
       minMax numMin val (<)

      Column _ NulVal -> modifyIORef' (numNullCount stats) (+1)

      _ -> return()
      where
          alpha = 0.02 -- EMA alpha 2/200
          minMax valFun val cmp = do
              cVal <- readIORef $ valFun stats
              when (cmp val cVal) $ writeIORef (valFun stats) val 

calcEMA :: RealFloat a => a -> a -> a -> a
calcEMA alpha old new = old + alpha * (new - old)

newLogEntryStats :: Schema -> IO LogEntryStats
newLogEntryStats (Schema cols) = do
    cnt <- newIORef 0
    colStats <- mapM newColumnStats cols
    return $ LogEntryStats colStats cnt

newColumnStats :: Column -> IO ColumnStats
newColumnStats (Column _ (TextVal _)) = do
    avg <- newIORef 0 
    lg <- newRef
    lVal <- newBsRef
    cnt <- newRef
    sh <- newRef
    sVal <- newBsRef
    return . TextCol $ TextStats avg lg lVal cnt sh sVal
    where 
        newRef = newIORef 0
        newBsRef = newIORef B.empty

newColumnStats (Column _ (NumberVal _)) = do
    avg <- newIORef 0
    mx <- newRef
    mn <- newRef
    cnt <- newIORef 0
    return . NumberCol $ NumberStats avg mx mn cnt
    where newRef = newIORef 0

newColumnStats (Column _ _) = error "Could not created conduit for column."

newStatsToByteStringConduit :: Schema -> IO (Conduit LogEntryStats IO B.ByteString)
newStatsToByteStringConduit schema = do
    lastEvent <- newIORef Nothing
    return $ conduit lastEvent
    where conduit lastEvent = do
            result <- await
            case result of
              Just  x -> do 
                  liftIO . writeIORef lastEvent $ Just x
                  conduit lastEvent

              _ -> do
                  x <- liftIO  $ readIORef lastEvent
                  bs <- liftIO $ showLogStats schema (fromJust x)
                  yield bs 
                  return ()

type BuilderMonad = StateT Builder IO

showLogStats :: Schema -> LogEntryStats -> IO B.ByteString              
showLogStats (Schema cols) stats = flip evalStateT mempty $ do
    appendBs $ fromByteString "Log Statistics" <> newLine

    cnt <- liftIO . readIORef $ statsEntryCount stats
    appendBs $ fromByteString "total count: " <> fromShow cnt <> newLine <> newLine

    let showCols = map showColumnStats cols 
    foldl (>>) (return ()) $ zipWith id showCols (statsColumnStats stats)

    builder <- get
    return $ toByteString builder 

showColumnStats :: Column -> ColumnStats -> BuilderMonad ()
showColumnStats (Column name _) (TextCol stats) = do
    appendColName name

    avg <- liftIO . readIORef $ textAverageLength stats
    appendBs $ fromByteString "average length: " <> fromShow avg <> newLine
    
    lg <- liftIO . readIORef $ textLongestVal stats
    appendBs $ fromByteString "longest values: " <> fromShow lg <> newLine

    lCnt <- liftIO . readIORef $ textLongestCount stats
    appendBs $ fromByteString "count of longest: " <> fromShow lCnt <> newLine

    sh <- liftIO . readIORef $ textShortestVal stats
    appendBs $ "shortest value: " <> fromShow sh <> newLine

    sCnt <- liftIO .readIORef $ textShortestCount stats
    appendBs $ "count of shortest: " <> fromShow sCnt <> newLine

    nulCnt <- liftIO. readIORef $ textNullCount stats
    appendBs $ "null count: " <> fromShow nulCnt <> newLine <> newLine

    return ()

showColumnStats (Column name _) (NumberCol stats) = do
    appendColName name

    avg <- liftIO . readIORef $ numAverage stats
    appendBs $ fromByteString "average: " <> fromShow avg <> newLine
    
    mx <- liftIO . readIORef $ numMax stats
    appendBs $ fromByteString "max: " <> fromSci mx <> newLine

    mn <- liftIO . readIORef $ numMin stats
    appendBs $ fromByteString "min: " <> fromSci mn <> newLine

    nulCnt <- liftIO. readIORef $ numNullCount stats
    appendBs $ "null count: " <> fromShow nulCnt <> newLine <> newLine

    return ()
    where 

fromSci :: Scientific -> Builder 
fromSci = fromShow . formatScientific Fixed (Just 3)

appendBs :: Builder -> StateT Builder IO ()
appendBs bs = modify $ flip (<>) bs

appendColName :: B.ByteString -> BuilderMonad ()
appendColName name = 
    appendBs $ fromByteString "Column (" <> fromByteString name <> "):" <> newLine

newLine :: Builder
newLine = fromByteString $ B.singleton _lf

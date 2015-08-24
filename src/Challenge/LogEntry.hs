{-# LANGUAGE OverloadedStrings #-}

module Challenge.LogEntry
    ( Column(..)
    , ColumnValue(..)
    , LogEntry(..)
    , Schema(..)
    , newLogEntryParser
    , schemaParser 
    ) where

import Control.Monad (liftM2)

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, scientific)
import Data.ByteString.Char8 (ByteString, empty, concat, unpack)
import Data.Functor ((<$>))
import Data.Scientific (Scientific)

import Prelude hiding (concat)

import qualified Data.Word8 as W 

data ColumnValue = TextVal ByteString | NumberVal Scientific | NulVal deriving Show

data Column = Column ByteString ColumnValue deriving Show

newtype LogEntry = LogEntry [Column] deriving Show

newtype Schema = Schema [Column] deriving Show

newLogEntryParser :: Schema -> Parser LogEntry
newLogEntryParser (Schema []) = error "Schema empty."

newLogEntryParser (Schema [s]) = do
    col <- typedColumnParser s
    return $ LogEntry [col]

newLogEntryParser (Schema (s:ss)) = do
    cols <- liftM2 (:) (typedColumnParser s) $ mapM build ss
    skipWhile isEndOfLine
    return $ LogEntry cols
    where
       build a = skipChar W._comma >> typedColumnParser a

typedColumnParser :: Column -> Parser Column
typedColumnParser (Column name value)  =
      case value of
         TextVal _ -> textColumnParser name 
         NumberVal _ -> numberColumnParser name 
         NulVal -> error "Invalid schema"
      
schemaParser :: Parser Schema
schemaParser = Schema <$> many1 columnParser 

columnParser :: Parser Column
columnParser = do
    skipQuote
    name <- takeTill W.isSpace 
    skip W.isSpace
    skipChar W._parenleft
    valType <- takeTill (W._parenright ==)
    skipChar W._parenright 
    skipQuote 
    skipWhile (W._comma ==)
    return $ Column name (columnValue valType)
    where 
        columnValue "text" = TextVal empty 
        columnValue "number" = NumberVal 0
        columnValue x = error . unpack $ concat ["Unsupported value type '", x, "'."]
        skipQuote = skipChar W._quotedbl

skipChar :: W.Word8 -> Parser ()
skipChar c = skip (c ==)

textColumnParser :: ByteString -> Parser Column
textColumnParser name = do
    text <- takeTill delim 
    return . Column name $ if empty == text then NulVal else TextVal text 
    where delim a = a == W._comma || isEndOfLine a

numberColumnParser :: ByteString -> Parser Column
numberColumnParser name = do
    num <- option NulVal $ NumberVal <$> scientific
    return $ Column name num 


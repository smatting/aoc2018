{-# LANGUAGE BangPatterns #-}

module Main where

import AOC.Lib

import Prelude hiding (lex)
import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1, string, eol)
import Data.List
import Data.Either
import Data.Vector (Vector)
import qualified Data.Vector as V


type Parser = Parsec Void Text

data Day
  = Day Int Int Int
  deriving (Eq, Ord, Show)

data Time
  = Time Int Int
  deriving (Eq, Ord, Show)

data Event
  = ShiftStart Integer
  | FallsAsleep
  | WakesUp
  deriving (Eq, Ord, Show)

data Entry
  = Entry Day Time Event
  deriving (Eq, Show)

instance Ord Entry
  where
    (Entry d1 t1 _) <= (Entry d2 t2 _) = (d1, t1) <= (d2, t2)


lex :: Parser a -> Parser a
lex = lexeme (space space1 empty empty)

parseDay :: Parser Day
parseDay = do
  year <- decimal
  string "-"
  month <- decimal
  string "-"
  day <- decimal
  return $ Day year month day

parseTime :: Parser Time
parseTime = do
  hour <- decimal
  string ":"
  minute <- decimal
  return $ Time hour minute

parseShiftStart :: Parser Event
parseShiftStart = do
  string "Guard #"
  id_ <- lex $ decimal
  string "begins shift"
  return $ ShiftStart id_

parseEvent :: Parser Event
parseEvent =
      (string "falls asleep" >> pure FallsAsleep)
  <|> (string "wakes up" >> pure WakesUp)
  <|> parseShiftStart

parseEntry :: Parser Entry
parseEntry = do
  string "["
  day <- parseDay
  string " "
  time <- parseTime
  string "]"
  string " "
  event <- parseEvent
  return $ Entry day time event

parseInput :: Parser [Entry]
parseInput = parseEntry `sepEndBy` eol

main =
  return ()


loadInput :: IO ()
loadInput = do
  txt <- readInputFile "day4.txt"
  let entries = fromRight [] $ runParser parseInput "" txt
  print entries

zeros :: Vector Int
zeros = V.fromList (replicate 60 0)

-- type P = Map (Day, Int) Int

-- computeP :: [Entry] -> P
-- computeP entries =
--   foldl' f Map.empty entries

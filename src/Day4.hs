{-# language BangPatterns #-}
{-# language OverloadedStrings #-}

module Day4

where

import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import Control.Monad.Combinators

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Maybe

import Data.List.Split
import Data.List

import Control.Lens
import Data.Ord


import Lib

type Parser = Parsec Void Text

newtype Guard = Guard Int
  deriving (Eq, Show, Ord)

data EventData =
    BeginsShift Guard
  | WakesUp
  | FallsAsleep
  deriving (Eq, Show)

data Event
  = Event { _eventTime :: UTCTime, _eventData :: EventData }
  deriving (Eq, Show)

timeParser :: Parser UTCTime
timeParser = do
  year <- decimal
  string "-"
  month <- decimal
  string "-"
  dom <- decimal
  string " "

  hours <- decimal
  string ":"
  minutes <- decimal

  return $
    UTCTime
      (fromGregorian year month dom)
      (timeOfDayToTime (TimeOfDay hours minutes 0))

evenDataParser :: Parser EventData
evenDataParser =
      pure FallsAsleep <* "falls asleep"
  <|> pure WakesUp <* "wakes up"
  <|> "Guard #" *> ((BeginsShift . Guard) <$> decimal) <* " begins shift"

eventParser :: Parser Event
eventParser =
  pure Event <* "[" <*> timeParser <* "] " <*> evenDataParser

eventsParser :: Parser [Event]
eventsParser =
  sepEndBy eventParser eol

withEvents :: Text -> ([Event] -> Text) -> Text
withEvents input f =
  case runParser eventsParser "" input of
    Left err -> T.pack (parseErrorPretty err)
    Right events -> f events

equating :: Eq b => (a -> b) -> (a -> a -> Bool)
equating f x y = f x == f y

tally :: Ord a => Eq a => [a] -> [(a, Int)]
tally = fmap (head &&& length) . group . sort

isBeginsShift :: Event -> Bool
isBeginsShift (Event time (BeginsShift _)) = True
isBeginsShift _                            = False

splitByGuards :: [Event] -> [[Event]]
splitByGuards = split (keepDelimsL (whenElt isBeginsShift))

guardBlock :: [Event] -> Maybe (Guard, [Event])
guardBlock all@((Event time (BeginsShift g)):xs) = Just (g, all)
guardBlock _ = Nothing

minute :: UTCTime -> Int
minute = fromIntegral . todMin . timeToTimeOfDay . utctDayTime

smin :: Event -> Event -> [Int]
smin (Event t1 FallsAsleep) (Event t2 WakesUp) =
  [minute t1 .. (minute t2 - 1)]
smin _ _ = []

sleepyMinutes :: [Event] -> [Int]
sleepyMinutes [] = []
sleepyMinutes (x:xs) =
  concat (zipWith smin (x:xs) xs)

code :: (Guard, (Int, Int)) -> Int
code ((Guard k), (m, _)) = k * m

tallyMinutesByGuard :: [Event] -> [(Guard, [(Int, Int)])]
tallyMinutesByGuard = 
        sortOn _eventTime
    >>> splitByGuards
    >>> mapMaybe guardBlock
    >>> sortBy (comparing fst)
    >>> groupBy (equating fst)
    >>> fmap (fst . head &&& tally . concatMap (sleepyMinutes . snd))
    >>> filter (not . null . snd)

solution :: PuzzlePart -> Text -> Text

solution Part1 input = 
  withEvents input $ (
        tallyMinutesByGuard
    >>> maximumBy (comparing (sum . fmap snd . snd))
    >>> fst &&& maximumBy (comparing snd) . snd
    >>> code
    >>> show
    >>> T.pack )

solution Part2 input = 
  withEvents input $ (
        tallyMinutesByGuard
    >>> concatMap (\(g, l) -> zip (repeat g) l)
    >>> maximumBy (comparing (snd . snd))
    >>> code
    >>> show
    >>> T.pack )

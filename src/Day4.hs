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


import Lib

type Parser = Parsec Void Text

newtype Guard = Guard Integer
  deriving (Eq, Show)

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

solution :: PuzzlePart -> Text -> Text
solution _ input = 
  withEvents input $ (
        sortOn _eventTime
    >>> splitByGuards
    >>> fmap guardBlock
    >>> concatMaybes
    >>> head
    >>> show
    >>> T.pack
    )

type MinuteCounts = Map Integer Integer

isBeginsShift :: Event -> Bool
isBeginsShift (Event time (BeginsShift _)) = True
isBeginsShift _                            = False

splitByGuards :: [Event] -> [[Event]]
splitByGuards = split (keepDelimsL (whenElt isBeginsShift))

guardBlock :: [Event] -> Maybe (Guard, Day, [Event])
guardBlock all@((Event time (BeginsShift g)):xs) = Just (g, utctDay time, all)
guardBlock _ = Nothing

concatMaybes :: [Maybe a] -> [a]
concatMaybes = concat . fmap maybeToList 
--

eventMinute :: Event -> Integer
eventMinute = fromIntegral . todMin . timeToTimeOfDay . utctDayTime . _eventTime

xx :: MinuteCounts -> Integer -> MinuteCounts
xx mc minute = over (at minute . non 0) (+1) mc


countMinutes :: [Event] -> MinuteCounts
countMinutes = foldl' xx M.empty . fmap eventMinute


-- countMinutes :: Guard -> Day -> [Event] -> MinuteCounts
-- countMinutes g events = _eventTime

{-# language BangPatterns #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Day7

where

import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import Control.Monad.Combinators

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Data.Either
import Data.List

import Control.Lens
import Data.Ord

import Data.Char

import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI

import Control.Monad
import Data.Traversable
import Data.Functor

import Lib


data IsPreFor = IsPreFor Char Char
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

preParser :: Parser IsPreFor
preParser = do
  "Step "
  c1 <- asciiChar
  " must be finished before step "
  c2 <- asciiChar
  " can begin."
  return $ IsPreFor c1 c2

parseInput :: Text -> [IsPreFor]
parseInput input =
  fromRight [] (runParser (sepEndBy preParser eol) "" input)

show' :: Show a => a -> Text
show' = T.pack . show

-- for each entry the set of prerequesites
type PreMap = Map Char (Set Char)

genPreMap :: [IsPreFor] -> PreMap
genPreMap = foldl' addPre M.empty
  where
    addPre :: PreMap -> IsPreFor -> PreMap
    addPre m (IsPreFor x y) =
      M.insert
        y
        ( case M.lookup x m of
            Nothing -> S.singleton y
            Just s -> S.insert x s
        )
        m

solution :: PuzzlePart -> Text -> Text
solution Part1 input = 
  show' $ parseInput input

solution Part2 input = input


runPuzzle :: (PuzzlePart -> Text -> Text) -> String -> PuzzlePart -> IO ()
runPuzzle solver day = runSolver solver ("inputs/" <> day <> ".txt")

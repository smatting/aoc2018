{-# language BangPatterns #-}
{-# language OverloadedStrings #-}


module Day6

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
import Data.Either
import Data.List

import Control.Lens
import Data.Ord

import Data.Char

import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI

import Lib


data Point = Point Int Int
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

pointParser :: Parser [Point]
pointParser =
  sepEndBy (Point <$> decimal <* ", " <*> decimal) eol

parseInput :: Text -> [Point]
parseInput input =
  fromRight [] (runParser pointParser "" input)

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f x1 x2 = f x1 == f x2

uniqueHead :: (a -> a -> Bool) -> [a] -> Maybe a
uniqueHead f [] = Nothing
uniqueHead f [x] = Just x
uniqueHead f (x1:x2:xs)
  | f x1 x2 = Nothing
  | otherwise = Just x1

strictMinimum :: (Ord b, Eq b) => (a -> b) -> [a] -> Maybe a
strictMinimum f = sortBy (comparing f) >>> uniqueHead (equating f)

distances :: [Point] -> Point -> [(Point, Int)]
distances anchors point = fmap (id &&& manhattan point) anchors

closestAnchor :: [Point] -> Point -> Maybe (Point, Int)
closestAnchor anchors = strictMinimum snd  . distances anchors

buildAnchorMap :: [Point] -> [Point] -> Map Point [(Point, Int)]
buildAnchorMap anchors = 
          fmap (id &&& closestAnchor anchors)
      >>> foldl' f M.empty
    where f m (p, Nothing) = m
          f m (p, Just (anchor, dist)) = M.insertWith (++) anchor [(p, dist)] m

data Rectangle
  = Rectangle Int Int Int Int
  deriving (Eq, Show)

boundingBox :: [Point] -> Rectangle
boundingBox points =
  let
    xs = fmap (\(Point x _) -> x) points
    ys = fmap (\(Point _ y) -> y) points
  in
    Rectangle (minimum xs) (minimum ys) (maximum xs) (maximum ys)

touches :: Rectangle -> Point -> Bool
touches (Rectangle xmin ymin xmax ymax) (Point x y) =
     x == xmin
  || x == xmax
  || y == ymin
  || y == ymax

rectanglePoints :: Rectangle -> [Point]
rectanglePoints (Rectangle xmin ymin xmax ymax) =
  [Point x y | x <- [xmin..xmax], y <- [ymin..ymax]]

removeInfinite :: Rectangle -> Map Point [(Point, Int)] -> Map Point [(Point, Int)]
removeInfinite bbox = M.filterWithKey f
  where f anchor = any (touches bbox . fst)

show' :: Show a => a -> Text
show' = T.pack . show

solution :: PuzzlePart -> Text -> Text
solution Part1 input = 
  let
    anchors = parseInput input
    bbox = boundingBox anchors
    points = rectanglePoints bbox
    amap' = buildAnchorMap anchors points
    amap = removeInfinite bbox amap'
    n = maximum (fmap length (M.elems amap))
  in
    -- show' $ fmap (id . fst &&& length . snd) (M.toList amap')
    show' amap

solution Part2 x = id x

s :: Text
s = "\
\1, 1\n\
\1, 6\n\
\8, 3\n\
\3, 4\n\
\5, 5\n\
\8, 9\n"

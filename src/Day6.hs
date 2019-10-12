{-# language BangPatterns #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Day6

where

import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Either
import Data.List

import Control.Lens
import Data.Ord

import Data.Char

import Lib


data Point = Point Int Int
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

data Anchor = Anchor { _anchorId :: Char, _anchorPoint :: Point}
  deriving (Eq, Ord, Show)

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
uniqueHead _ [] = Nothing
uniqueHead _ [x] = Just x
uniqueHead f (x1:x2:_)
  | f x1 x2 = Nothing
  | otherwise = Just x1

strictMinimum :: (Ord b, Eq b) => (a -> b) -> [a] -> Maybe a
strictMinimum f = sortBy (comparing f) >>> uniqueHead (equating f)

distances :: [Anchor] -> Point -> [(Anchor, Int)]
distances anchors point = fmap (id &&& manhattan point . _anchorPoint) anchors

closestAnchor :: [Anchor] -> Point -> Maybe (Anchor, Int)
closestAnchor anchors = strictMinimum snd  . distances anchors

buildAnchorMap :: [Anchor] -> [Point] -> Map Anchor [(Point, Int)]
buildAnchorMap anchors = 
          fmap (id &&& closestAnchor anchors)
      >>> foldl' f M.empty
    where f m (_, Nothing) = m
          f m (p, Just (anchor, dist)) = M.insertWith (++) anchor [(p, dist)] m

data Rectangle
  = Rectangle Int Int Int Int
  deriving (Eq, Show)

boundingBox :: Int -> [Point] -> Rectangle
boundingBox padding points =
  let
    xs = fmap (\(Point x _) -> x) points
    ys = fmap (\(Point _ y) -> y) points
  in
    Rectangle (minimum xs - padding) (minimum ys - padding) (maximum xs + padding) (maximum ys + padding)

touches :: Rectangle -> Point -> Bool
touches (Rectangle xmin ymin xmax ymax) (Point x y) =
     x == xmin
  || x == xmax
  || y == ymin
  || y == ymax

rectanglePoints :: Rectangle -> [Point]
rectanglePoints (Rectangle xmin ymin xmax ymax) =
  [Point x y | x <- [xmin..xmax], y <- [ymin..ymax]]

removeInfinite :: Rectangle -> Map Anchor [(Point, Int)] -> Map Anchor [(Point, Int)]
removeInfinite bbox = M.filterWithKey f
  where f _ = not . any (touches bbox . fst)

show' :: Show a => a -> Text
show' = T.pack . show

enumeratePoints :: [Point] -> [Anchor]
enumeratePoints = 
  zipWith Anchor (cycle ['A'..'Z'])

invertMap :: Ord b => Map a [b] -> Map b a 
invertMap =
  M.toList
  >>> concatMap (uncurry (fmap . flip (,)))
  >>> M.fromList

anchorMapToPoints :: Map Anchor [(Point, Int)] -> Map Point Anchor
anchorMapToPoints = M.mapKeys fst . invertMap

showField :: Rectangle -> Map Point Anchor -> Text
showField (Rectangle xmin ymin xmax ymax) m =
  T.intercalate "\n" $
    [ymin..ymax] <&> \y ->
      T.pack $ [xmin..xmax] <&> \x ->
        let p = Point x y in
        case M.lookup p m of
          Nothing -> '.'
          Just (Anchor char ap) ->
            if ap /= p
               then toLower char
               else char

sumDistance :: [Anchor] -> Point -> Int
sumDistance anchors point =
  sum $ fmap (manhattan point . _anchorPoint) anchors

solution :: PuzzlePart -> Text -> Text
solution Part1 input = 
  let
    anchors = (enumeratePoints . parseInput) input
    bbox = boundingBox 1 (fmap _anchorPoint anchors)
    points = rectanglePoints bbox 
    amap' = buildAnchorMap anchors points
    amap = removeInfinite bbox amap'
    n = maximum (fmap length (M.elems amap))
  in
    show' n

solution Part2 input =
  let
    anchors = (enumeratePoints . parseInput) input
    bbox = boundingBox 1 (fmap _anchorPoint anchors)
    points = rectanglePoints bbox 
  in
    show' $ length $ filter ((< 10000) . sumDistance anchors) points


dummyPart1 = T.putStrLn $ solution Part1 s
dummyPart2 = T.putStrLn $ solution Part2 s

s :: Text
s = "\
\1, 1\n\
\1, 6\n\
\8, 3\n\
\3, 4\n\
\5, 5\n\
\8, 9\n"

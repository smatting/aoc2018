{-# LANGUAGE BangPatterns #-}

module Main where

import Advent

import Data.Maybe
import Data.Function
import Data.List

import Data.Void

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1, string, eol, upperChar, newline)

import Data.Map.Strict (Map)
import qualified Data.Map as Map

type Label = Char
type Point = (Int, Int)
type Dimensions = (Int, Int)

bbox :: [Point] -> (Point, Point)
bbox l =
  let
    xs = fmap fst l
    ys = fmap snd l
  in
    ((minimum xs - 100, minimum ys - 100)
    ,(maximum xs + 100, maximum ys + 100))

relativize :: (Point, Point) -> [Point] -> ([Point], Dimensions)
relativize ((xmin, ymin), (xmax, ymax)) l =
  (fmap f l, (xmax-xmin, ymax-ymin))
    where
      f (x, y) = (x-xmin, y-ymin)

coords :: Int -> Int -> [Point]
coords width height = [ (x, y) | y <- [0..height-1], x <- [0..width-1] ]

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

nearest :: [(Int, Point)] -> Point -> (Int, Point)
nearest l p =
  let
    sorted = sortBy (compare `on` (manhattan p . snd)) l
  in
    (fromJust $ fst <$> listToMaybe sorted, p)

touchesBorder :: Dimensions -> Point -> Bool
touchesBorder (width, height) (x, y) = 
  x == 0 || x == width-1 || y == 0 || y == height-1

area :: Dimensions -> [Point]  -> Maybe Int
area dims points
  | any (touchesBorder dims) points = Nothing
  | otherwise = Just $ length points

type Parser = Parsec Void Text
  
pPoint :: Parser Point
pPoint = do
  x <- decimal
  string ", "
  y <- decimal
  return (x, y)
  
pInput :: Parser [Point]
pInput = pPoint `sepEndBy` newline

loadInput :: IO [Point]
loadInput = do
  input <- readInputFile "day6.txt"
  return $ fromJust $ parseMaybe pInput input

prepareInput :: [Point] -> ([(Int, Point)], Dimensions)
prepareInput points' =
  let
    bb = bbox points'
    (points, dims) = relativize bb points'
 in 
   (zip [1..] points, dims)

groupToMap :: Ord a => [(a, b)] -> Map a [b]
groupToMap l = foldr (uncurry (Map.insertWith (++))) Map.empty (fmap f l)
  where
    f (x, y) = (x, [y])

example = fromJust $ parseMaybe pInput $ "1, 1\n\
\1, 6\n\
\8, 3\n\
\3, 4\n\
\5, 5\n\
\8, 9"

small :: [(Int, Int)]
small = [(2,2), (5,3)]

solution points' =
  let (points, dims) = prepareInput points'
      labels = fmap (nearest points) ((uncurry coords) dims)
      m = groupToMap labels
      -- m' = Map.map (area dims) m
      m' = fmap (area dims) (Map.elems m)
   in maximum (catMaybes m')

main :: IO ()
main = do
  points' <- loadInput
  let s = solution points'
  print s

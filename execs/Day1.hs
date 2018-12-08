module Main (main)

where

import Advent

import Prelude hiding (lines)
import qualified Data.Text as T
import Data.Text.Read

import Data.Either (rights)

import Data.Set (Set)
import qualified Data.Set as S

readInput = readInputFile "day1.txt"

parseInput =
  fmap fst . rights . fmap (signed decimal) . T.lines

solution1 = do
  x <- parseInput <$> readInput
  print $ sum x

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = f S.empty
  where
    f seen []     = Nothing
    f seen (x:xs) | x `S.member` seen = Just x
                  | otherwise = f (S.insert x seen) xs

solution2 = do
  x <- parseInput <$> readInput
  let freqs = scanl1 (+) (cycle x)
  print $ firstRepeat freqs

main = do
  solution1
  solution2

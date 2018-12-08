module Main (main)

where

import Advent

import Prelude hiding (lines)
import qualified Data.Text as T
import Data.Text.Read
import Data.List (transpose)

import Data.Either (rights)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map as Map

import Data.Foldable (foldr, foldl', asum)

readInput = readInputFile "day2.txt"

parseInput =
  fmap T.unpack . T.lines

valueCounts :: Ord a => [a] -> Map a Int
valueCounts s =
  foldl' f Map.empty s
    where
      f m k = Map.insertWith (+) k 1 m

solution1 = do
  lines <- parseInput <$> readInput
  let l = fmap (Set.fromList . Map.elems . valueCounts) lines
  print $ countDuplicityOrders 2 l * countDuplicityOrders 3 l
  where
    countDuplicityOrders k ds = sum $ fmap (boolToInt . Set.member k) ds
    boolToInt b = if b then 1 else 0

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = f Set.empty
  where
    f seen []     = Nothing
    f seen (x:xs) | x `Set.member` seen = Just x
                  | otherwise = f (Set.insert x seen) xs

dropLetters :: String -> [String]
dropLetters s = f "" s
  where
    f s []     = []
    f s (x:xs) = (s ++ xs) : (f (s ++ [x]) xs) 

solution2 = do
  lines <- parseInput <$> readInput
  print $ (asum . fmap firstRepeat . transpose . fmap dropLetters) lines

main = do
    solution1
    solution2

module Day2

where

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

import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow

import Lib


parseInput =
  fmap T.unpack . T.lines

valueCounts :: Ord a => [a] -> Map a Int
valueCounts s =
  foldl' f Map.empty s
    where
      f m k = Map.insertWith (+) k 1 m

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


solution :: PuzzlePart -> Text -> Text
solution Part1 = parseInput
                 >>> fmap (Set.fromList . Map.elems . valueCounts)
                 >>> countDuplicityOrders 2 &&& countDuplicityOrders 3 
                 >>> uncurry (*)
                 >>> show
                 >>> T.pack
  where
    countDuplicityOrders k ds = sum $ fmap (boolToInt . Set.member k) ds
    boolToInt b = if b then 1 else 0

solution Part2 = parseInput
                 >>> fmap dropLetters
                 >>> transpose
                 >>> fmap firstRepeat
                 >>> asum
                 >>> show
                 >>> T.pack

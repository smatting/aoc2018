module Day2

where

import Prelude
import Data.List (transpose)


import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Foldable (foldl', asum)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow

import Lib


parseInput =
  fmap T.unpack . T.lines

valueCounts :: Ord a => [a] -> Map a Int
valueCounts =
  foldl' f Map.empty
    where
      f m k = Map.insertWith (+) k 1 m

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = f Set.empty
  where
    f _    []     = Nothing
    f seen (x:xs) | x `Set.member` seen = Just x
                  | otherwise = f (Set.insert x seen) xs

dropLetters :: String -> [String]
dropLetters = f ""
  where
    f _ []     = []
    f s (x:xs) = (s ++ xs) : f (s ++ [x]) xs 


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

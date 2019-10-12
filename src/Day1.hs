module Day1 (solution)

where

import Lib

import qualified Data.Text as T
import Data.Text.Read
import Data.Either (rights)
import qualified Data.Set as S

import Data.Text (Text)
import Control.Arrow


firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = f S.empty
  where
    f _    []     = Nothing
    f seen (x:xs) | x `S.member` seen = Just x
                  | otherwise = f (S.insert x seen) xs

parseInput =
  fmap fst . rights . fmap (signed decimal) . T.lines


solution :: PuzzlePart -> Text -> Text

solution Part1 = parseInput
                 >>> sum
                 >>> show
                 >>> T.pack

solution Part2 = parseInput
                 >>> scanl1 (+)
                 >>> cycle
                 >>> firstRepeat
                 >>> show
                 >>> T.pack

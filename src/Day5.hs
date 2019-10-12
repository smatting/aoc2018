{-# language PartialTypeSignatures #-}


module Day5

where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow
import qualified Data.CaseInsensitive as CI

import Lib

red :: String -> String
red [] = []
red [x] = [x]
red (x1:x2:xs)
  | unitEqual x1 x2 = red xs
  | otherwise = x1 : red (x2:xs)

reduce :: String -> String
reduce s =
  let s' = red s
   in if length s' /= length s
         then reduce s'
         else s

unitEqual :: Char -> Char -> Bool
unitEqual c1 c2 =
  CI.mk c1 == CI.mk c2

filteredPolymers :: String -> [String]
filteredPolymers input = [filter (not . unitEqual x) input | x <- ['a' .. 'z']]

solution :: PuzzlePart -> Text -> Text
solution Part1 =
      T.strip
  >>> T.unpack
  >>> reduce 
  >>> length
  >>> show
  >>> T.pack

solution Part2 =
      T.strip
  >>> T.unpack
  >>> filteredPolymers
  >>> fmap (length . reduce)
  >>> minimum
  >>> show
  >>> T.pack

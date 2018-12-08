{-# LANGUAGE BangPatterns #-}

module Main where

import AOC.Lib

import Prelude hiding (lex)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1, string, eol)
import Data.List
import Data.Either
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

import Data.Char

data Node
  = Node
      (Vector Node)
      [Int]
  deriving (Show, Eq)

loadInput :: IO (Vector Int)
loadInput = do
  (V.fromList . fmap read . fmap T.unpack . T.words) <$> readInputFile "day8.txt"

sliceFrom :: Int -> Vector a -> Vector a
sliceFrom i xs = V.unsafeSlice i (V.length xs - i) xs

parseTree' :: Vector Int -> (Int, Node)
parseTree' xs =
  let
    nchildren = xs ! 0
    nmeta     = xs ! 1
    f (offset, nodes) i = 
      let
        xs' = sliceFrom offset xs
        (length, node) = parseTree' xs'
      in
        (offset + length, node:nodes)
    (lastOffset, childrenRev) = foldl' f (2, []) [1..nchildren]
    children = reverse childrenRev
    meta     = V.toList (V.unsafeSlice lastOffset nmeta xs)
  in
    (lastOffset + nmeta, Node (V.fromList children) meta)

parseTree :: Vector Int -> Node
parseTree = snd . parseTree'

sumMeta :: Node -> Int
sumMeta (Node children meta)
  = sum (fmap sumMeta children) + sum meta

sumMeta2 :: Node -> Int
sumMeta2 (Node children meta)
  | V.null children = sum meta
  | otherwise = foldl' f 0 meta
  where
    f s metaEntry
      = s + maybe 0 sumMeta2 (children !? (metaEntry - 1))

main = do
  xs <- loadInput
  let tree = parseTree xs
  print $ sumMeta tree
  print $ sumMeta2 tree

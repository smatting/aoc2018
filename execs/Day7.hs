{-# LANGUAGE BangPatterns #-}

module Main where

import Advent

import Data.List

import Data.Void

import Data.Maybe

import Control.Monad

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1, string, eol, upperChar, newline)

main = return ()

type Before = (Char, Char)

starts :: [Char] -> [Before] -> [Char]
starts remaining l =
  let 
    set = Set.fromList (fmap snd l)
    unique = Set.toList . Set.fromList
    candidates = unique $ filter (not . (`Set.member` set)) remaining
  in
    sort candidates

removeDest :: Char -> [Before] -> [Before]
removeDest k l = filter ((/=) k . fst) l

f :: [Char] -> [Before] -> Maybe [Char]
f res befores = 
  let
    remaining = filter (not . (`elem` res)) allLetters
  in
    if null remaining
       then return $ reverse res
       else do
          x <- listToMaybe (starts remaining befores)
          f (x:res) (removeDest x befores)

type Parser = Parsec Void Text

pBefore :: Parser Before
pBefore = do
  string "Step " 
  c1 <- upperChar
  string " must be finished before step "
  c2 <- upperChar
  string " can begin."
  return $ (c1, c2)

pInput :: Parser [Before]
pInput = pBefore `sepEndBy` newline

loadInput :: IO [Before]
loadInput = do
  input <- readInputFile "day7.txt"
  return $ fromJust $ parseMaybe pInput input

solution :: [Before] -> Maybe [Char]
solution befores = f [] befores

allLetters = ['A'..'Z']

solution1 :: IO ()
solution1 = do
  x <- solution <$> loadInput
  print x



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
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Data.Char

loadInput :: IO String
loadInput = do
  (T.unpack . head . T.lines) <$> readInputFile "day5.txt"

squashStep :: (Char -> Char -> Bool) -> String -> (Bool, String)
squashStep p t = g False [] (zip t (tail t))
  where
    g sq r ((x, y):p2:p3:xs) =
      if (p x y)
         then g True r (p3:xs)
         else g sq (x:r) (p2:p3:xs) 
    g sq r ([(x, y),p2]) =
      if (p x y)
         then g True (y:r) []
         else g sq (x:r) (p2:[]) 
    g sq r ([(x, y)]) =
      if (p x y)
         then g True (r) []
         else g sq (y:x:r) []
    g sq r [] = (sq, reverse r)

pred1 :: Char -> Char -> Bool
pred1 c1 c2 = 
  (isUpper c1 /= isUpper c2) && (toUpper c1 == toUpper c2)

squashAll' p False s = s
squashAll' p True s = (uncurry (squashAll' p)) (squashStep p s)

reducePoly1 = squashAll' pred1 True

solution1 = do
  l <- reducePoly1 <$> loadInput
  print $ length $ l

main = return ()

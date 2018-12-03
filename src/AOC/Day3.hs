{-# LANGUAGE BangPatterns #-}

module AOC.Day3

where

import AOC.Lib

import Prelude hiding (lex)
import Data.Array.Repa as R
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1, string)
import Data.Void
import Data.Text (Text)
import Data.Either
import Data.Foldable

d2 :: Int -> Int -> DIM2
d2 j i = Z :. j :. i

zeros :: DIM2 -> Array U DIM2 Int
zeros shape@(Z :. rows :. cols)
  = fromListUnboxed shape (replicate (rows*cols) 0)

type Range = (DIM2, DIM2)

range :: (Int,Int) -> (Int, Int) -> Range
range (startI, startJ) (stopI, stopJ)
  = (d2 startI startJ, d2 (stopI) (stopJ))

--mapInsideRange :: Range -> (Int -> Int) -> Array U DIM2 Int -> Array D DIM2 Int
mapInsideRange :: (Source r a) => Range -> (a -> a) -> Array r DIM2 a -> Array D DIM2 a
mapInsideRange (!idxStart, !idxStop) !f !arr = deepSeqArray arr $ R.traverse arr id g
  where
    g lookup idx
      | inShapeRange idxStart idxStop idx = f (lookup idx)
      | otherwise = lookup idx

arr = zeros (d2 10 8)

arr2 :: Array U DIM2 Int
arr2 = computeS $ mapInsideRange (range (0,0) (2,2)) (+1) arr

type Parser = Parsec Void Text
type Id = Int

data Claim = Claim Id Int Int Int Int
  deriving (Show, Eq)

lex :: Parser a -> Parser a
lex = lexeme (space space1 empty empty)

parseClaim :: Parser Claim
parseClaim = do
  lex $ string "#"
  id_ <- lex $ decimal
  lex $ string "@"
  x <- lex $ decimal
  string ","
  y <- lex $ decimal
  lex $ string ":"
  width <- lex $ decimal
  string "x"
  height <- lex $ decimal
  return $ Claim id_ x y width height

parseInput = many parseClaim <* eof

loadInput :: IO [Claim]
loadInput = do
  txt <- readInputFile "day3.txt"
  return $ fromRight [] (runParser parseInput "" txt)

claimRange :: Claim -> Range
claimRange (Claim _ x y width height) = ((d2 x y), (d2 (x + width) (y + height)))

claim = parseTest' (parseClaim <* eof) "#296 @ 172,604: 11x16"

applyClaimed claims =
  let
    arr = zeros (d2 1000 1000)
  in
    foldl' f arr (fmap claimRange claims)
  where
    f arr r = computeS (mapInsideRange r (+1) arr)


applyClaimed2 ! claims =
  let
    !arr = zeros (d2 1000 1000)
  in
    deepSeqArray arr $ foldlM f arr (fmap claimRange claims)
  where
    f (!arr) (!r) =  computeP $ deepSeqArray arr (mapInsideRange r (+1) arr)

solution1 = do
  claims <- loadInput
  x <- applyClaimed2 (take 2 claims)
  print $ sumAllS x

-- bar :: Array U DIM2 Int
bar :: IO Int
bar =
  let
    arr = zeros (d2 1000 1000)
    f arr = (mapInsideRange (range (0,0) (2,2)) (+1) arr)
  in
    -- sumAllP $ f (f (f arr))
    sumAllP $ f arr


-- -- #296 @ 172,604: 11x16


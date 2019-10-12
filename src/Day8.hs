module Day8
where

import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer (decimal, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import Control.Monad (replicateM)
import Data.Either
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.Maybe

import Lib

type Parser = Parsec Void Text

type Metadata = [Int]

data Node = Node Metadata [Node]
  deriving (Show, Eq) 

parseInt :: Parser Int
parseInt = decimal

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (space space1 empty empty)

parseNode :: Parser Node
parseNode = do
  nChildren <- lexeme parseInt
  nMeta <- lexeme parseInt
  children <- replicateM nChildren (lexeme parseNode)
  meta <- replicateM nMeta (lexeme parseInt)
  return (Node meta children)

parseInput :: Text -> Node
parseInput input =
  fromRight (error "whoops") (runParser parseNode "" input)

sumMeta ::Node -> Int
sumMeta node = sum (metadata node)
  where
    metadata (Node md children) =
      md ++ concatMap metadata children

nodeValue :: Node -> Int
nodeValue (Node md []) = sum md

nodeValue (Node md children) =
  let vals = V.fromList (fmap nodeValue children)
   in sum (fmap (lookup vals) md)
  where
    lookup :: Vector Int -> Int -> Int
    lookup vals i = fromMaybe 0 (vals !? (i - 1))

sample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
sampleSolution1 = sumMeta (parseInput sample)
sampleSolution2 = nodeValue (parseInput sample)

solution :: PuzzlePart -> Text -> Text
solution Part1 input = 
  showText $ sumMeta (parseInput input)

solution Part2 input = 
  showText $ nodeValue (parseInput input)

main1 = runPuzzle solution "day8" Part1
main2 = runPuzzle solution "day8" Part2

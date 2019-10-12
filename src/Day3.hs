{-# language OverloadedStrings #-}

module Day3

where

import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe

import Lib

type Parser = Parsec Void Text

data Point = Point !Int !Int
  deriving (Eq, Show, Ord)

data Dimension = Dimension !Int !Int
  deriving (Eq, Show)

data Patch =
  Patch Int Point Dimension
  deriving (Eq, Show)

parsePatch :: Parser Patch
parsePatch = do
  string "#"
  n <- decimal
  string " @ "
  point <- Point <$> decimal <* string "," <*> decimal
  string ": "
  dim <- Dimension <$> decimal <* string "x" <*> decimal
  return $ Patch n point dim

parsePatches :: Parser [Patch]
parsePatches =
  sepEndBy parsePatch eol

withPatches :: Text -> ([Patch] -> Text) -> Text
withPatches input f =
  case runParser parsePatches "" input of
    Left err -> T.pack (errorBundlePretty err)
    Right patches -> f patches

type Counts = Map Point Int

points :: Patch -> [Point]
points (Patch _ (Point dx dy) (Dimension w h)) = do
  x <- [0..(w-1)]
  y <- [0..(h-1)]
  return $ Point (x + dx) (y + dy)

countPoint :: Point -> Counts -> Counts
countPoint point counts =
  counts & at point . non 0 +~ 1

countPatch :: Patch -> Counts -> Counts
countPatch patch counts = foldr countPoint counts $ points patch

countPatches :: Counts -> [Patch] -> Counts
countPatches = foldr countPatch

solution :: PuzzlePart -> Text -> Text

solution Part1 input =
  withPatches input
    (    countPatches M.empty
     >>> M.elems
     >>> filter (>= 2)
     >>> length
     >>> show
     >>> T.pack )
  
solution Part2 input =
  withPatches input $ \patches ->
    let
      cnt = countPatches M.empty patches
    in
      T.pack . show $ filter (patchIsUnique cnt) patches

patchIsUnique :: Counts -> Patch -> Bool
patchIsUnique counts =
      points
  >>> mapMaybe (`M.lookup` counts)
  >>> all (== 1)

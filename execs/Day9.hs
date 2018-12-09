{-# LANGUAGE BangPatterns #-}

module Main where

import Advent

import Prelude hiding (lex, length)
import Data.Monoid
import Data.Foldable hiding (length)
import Data.List as List hiding (length)
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1, string, eol)

import Data.Function

import Data.Sequence

type Turn = Int
newtype Player = Player Int
  deriving (Show, Eq, Ord)
newtype Point = Point Int
  deriving (Show, Eq, Ord)
type Circle = Seq Int
newtype Pos = Pos Int
  deriving (Show, Eq)

type GameState = (Pos, Circle)

posAdd :: Int -> GameState -> Pos
posAdd delta (Pos k, circle) =
  Pos $ (k + delta) `mod` length circle

regularStep :: Turn -> GameState -> GameState
regularStep t state@(_, circle) =
  let
    pos'@(Pos k) = posAdd 2 state
    circle' = insertAt k t circle
  in (pos', circle')
  
m23step :: Turn -> GameState -> (GameState, Int)
m23step t state@(_, circle) =
  let
    pos'@(Pos k) = posAdd (-7) state
    x = circle `index` k
    circle' = deleteAt k circle
  in
    ((pos', circle'), x)
    
step :: Turn -> Player -> GameState -> (GameState, [(Player, Point)])
step turn player state =
  if turn `mod` 23 == 0
    then
      let (state', k) = m23step turn state
       in (state', [(player, Point k), (player, Point turn)])
    else
      (regularStep turn state, [])
    
initialState :: GameState
initialState = ((Pos 0), fromList [0])

playGame :: Int -> Int -> (GameState, [(Player, Point)])
playGame nplayers nsteps =
  foldl' f (initialState, []) [1..nsteps]
    where
      f (state, log) turn =
        let
          player = Player (((turn - 1) `mod` nplayers) + 1)
          (state', log') = step turn player state
        in 
          (state', log <> log')
          
collectPoints :: [(Player, Point)] -> Map Player Point
collectPoints l  = foldr
              (uncurry (Map.insertWith (\(Point p1) (Point p2) -> Point (p1 + p2)))) 
              Map.empty
              l
          
winner :: Map Player Point -> Maybe (Player, Point)
winner m =
  let sorted = List.reverse $ List.sortBy (compare `on` snd) (Map.toList m)  
   in listToMaybe sorted

finishGame nplayer nsteps
  = let (state', log) = playGame nplayer nsteps
     in (winner . collectPoints) log

solution1 =
  print $ finishGame 423 71944

main = do
  solution1


module Main (main)
where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath

import Lib

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6


puzzlePartParser :: Parser PuzzlePart
puzzlePartParser =
  flag Part1 Part2 (long "bonus")

runSolver :: (PuzzlePart -> Text -> Text) -> FilePath -> PuzzlePart -> IO ()
runSolver solver filename part =
  solver part <$> T.readFile filename >>= T.putStrLn

mk :: String -> (PuzzlePart -> Text -> Text) -> Mod CommandFields (IO ())
mk day solver =
  command
    day
    (info (Lib.runPuzzle solver day <$> puzzlePartParser) fullDesc) 

parseProgram :: Parser (IO ())
parseProgram =
  subparser
    (    mk "day1" Day1.solution 
      <> mk "day2" Day2.solution
      <> mk "day3" Day3.solution
      <> mk "day4" Day4.solution
      <> mk "day5" Day5.solution
      <> mk "day6" Day6.solution
    )

main :: IO ()
main = join $ execParser (info parseProgram fullDesc)

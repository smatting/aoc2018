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


puzzlePartParser :: Parser PuzzlePart
puzzlePartParser =
  flag Part1 Part2 (long "bonus")


runSolver :: (PuzzlePart -> Text -> Text) -> FilePath -> PuzzlePart -> IO ()
runSolver solver filename part =
  T.readFile filename >>= (pure . solver part) >>= T.putStrLn


mk :: String -> (PuzzlePart -> Text -> Text) -> Mod CommandFields (IO ())
mk day solver =
  command
    day
    (info (runSolver solver ("inputs/" <> day <> ".txt") <$> puzzlePartParser) fullDesc) 


parseProgram :: Parser (IO ())
parseProgram =
  subparser
    (    mk "day1" Day1.solution 
      <> mk "day2" Day2.solution
      <> mk "day3" Day3.solution
    )


main :: IO ()
main = join $ execParser (info parseProgram fullDesc)

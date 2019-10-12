module Lib
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data PuzzlePart = Part1 | Part2 deriving (Eq, Show)

runSolver :: (PuzzlePart -> Text -> Text) -> FilePath -> PuzzlePart -> IO ()
runSolver solver filename part =
  T.readFile filename >>= (pure . solver part) >>= T.putStrLn

runPuzzle :: (PuzzlePart -> Text -> Text) -> String -> PuzzlePart -> IO ()
runPuzzle solver day = runSolver solver ("inputs/" <> day <> ".txt")

showText :: Show a => a -> Text
showText = T.pack . show

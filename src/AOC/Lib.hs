module AOC.Lib

where

import Paths_aoc2018 (getDataFileName)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))

readInputFile :: String -> IO Text
readInputFile fn = do
  T.readFile ("inputs/" <> fn)


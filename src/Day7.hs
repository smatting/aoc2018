{-# language TupleSections #-}
{-# language QuasiQuotes #-}

module Day7

where

import Text.Megaparsec (Parsec, runParser, sepEndBy)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Either
import Data.List
import Text.RawString.QQ
import Control.Arrow (first)

import Lib


data IsPreFor = IsPreFor Char Char
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

preParser :: Parser IsPreFor
preParser = do
  "Step "
  c1 <- asciiChar
  " must be finished before step "
  c2 <- asciiChar
  " can begin."
  return $ IsPreFor c1 c2

parseInput :: Text -> [IsPreFor]
parseInput input =
  fromRight [] (runParser (sepEndBy preParser eol) "" input)

-- for each entry the set of prerequesites
type PreMap =
  Map
    Char       -- ^ x
    (Set Char) -- ^ prequesites for x

genPreMap :: [IsPreFor] -> PreMap
genPreMap ipfs = foldl' addPre init ipfs
  where
    init = M.fromList $ fmap (, S.empty) (allChars ipfs)

    addPre m (IsPreFor x y) =
      M.insertWith S.union y (S.singleton x) m

    allChars = S.toList . S.fromList . go
      where
        go (IsPreFor a b : xs) = a : b : allChars xs
        go [] = []

findPath :: PreMap -> [Char]
findPath m = reverse $ go [] m
  where
    go chars m
      | M.null m  = chars
      | otherwise =
          case sort (filter (isAvailable m) (M.keys m)) of
            (c:_) -> go (c:chars) (removeDep c m)
            [] -> chars

isAvailable :: PreMap -> Char -> Bool
isAvailable m c =
  maybe True S.null (M.lookup c m)

removeDep :: Char -> PreMap -> PreMap
removeDep c m = M.delete c $ M.map (S.delete c) m

data Workers
  = Workers
      Int           -- ^ available workers
      [(Int, Char)] -- ^ remaining workload, shortest first
  deriving (Show)

initWorkers n = Workers n []

hasCapacity :: Workers -> Bool
hasCapacity (Workers 0 _) = False
hasCapacity (Workers _ _) = True

secs :: Char -> Int
secs c = 60 + fromEnum c - fromEnum 'A' + 1

insertJob :: Char -> Workers -> Workers
insertJob char (Workers n xs) =
  Workers (n - 1) (sort ((secs char, char):xs))

insertMost :: [Char] -> Workers -> Workers
insertMost chars workers = foldl' go workers chars
  where
    go workers char
      | hasCapacity workers = insertJob char workers
      | otherwise           = workers

work :: Workers -> (Int, [Char], Workers)
work (Workers n xs@((k, _):_)) =
  let
    xs'        = fmap (first (flip (-) k)) xs
    (xs1, xs2) = partition ((== 0) . fst) xs'
  in
    ( k
    , sort (fmap snd xs1)
    , Workers (n + length xs1) (sort xs2)) 

work w = (0, [], w)

parallelTime :: PreMap -> Workers -> Int
parallelTime m workers = go m workers 0 
  where
    go m workers n
      | M.null m = n
      | otherwise =
          let
            chars                        = sort (filter (isAvailable m) (M.keys m))
            (k, charsComplete, workers') = work (insertMost chars workers)
            m'                           = foldl' (flip removeDep) m charsComplete
          in
            go m' workers' (n + k)

solution :: PuzzlePart -> Text -> Text
solution Part1 input = 
  showText $ findPath (genPreMap (parseInput input))

solution Part2 input = 
  showText $ parallelTime (genPreMap (parseInput input)) (initWorkers 5)

sol1 = runPuzzle solution "day7" Part1
sol2 = runPuzzle solution "day7" Part2

--------------------------------------------------------------------------------


dummyInput :: Text
dummyInput = T.strip [r|
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
|]

sampleSolution1 = findPath $ genPreMap (parseInput dummyInput)

dummySolution2 =
  let 
    m = genPreMap (parseInput dummyInput)
  in
    parallelTime m (initWorkers 2)

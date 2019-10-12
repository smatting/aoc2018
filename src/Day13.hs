{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Day13

where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char (Char)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens 
import Text.RawString.QQ
import Control.Monad ((>=>), foldM)
import Data.Either (fromRight)
import Data.List (sortOn, concatMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))

import Lib

data TrackElt
  = Hor    -- -
  | Vert   -- |
  | Slash  -- /
  | BSlash -- \
  | Cross  -- +
  deriving (Show, Eq, Ord)
  
data Pos = Pos { _x :: Int, _y :: Int}
  deriving (Eq)

instance Ord Pos
  where
    Pos x1 y1 <= Pos x2 y2 = (y1, x1) <= (y2, x2)

makeLenses ''Pos

instance Show Pos
  where
    show (Pos x y) = show x ++ "," ++ show y

data Dir =
  N | E | S | W
  deriving (Show, Eq, Ord)

data Turn =
  LeftTurn | RightTurn | NoTurn
  deriving (Show, Eq, Ord)

data Cart = Cart { _cartPos :: Pos, _cartDir :: Dir, _cartTurn :: Turn }
  deriving (Show, Eq, Ord)
makeLenses ''Cart

type TrackMap = Map Pos TrackElt

parseGame :: Text -> Either Text (TrackMap, [Cart])
parseGame s = foldM go (M.empty, []) (coordChars s)
  where
    go (tm, carts) (pos, char) =
      let
        op = case char of
               '-' -> addElt pos Hor
               '|' -> addElt pos Vert
               '/' -> addElt pos Slash
               '\\' -> addElt pos BSlash
               '+' -> addElt pos Cross

               '^' -> addCart pos N >=> addElt pos Vert
               '>' -> addCart pos E >=> addElt pos Hor
               'v' -> addCart pos S >=> addElt pos Vert
               '<' -> addCart pos W >=> addElt pos Hor
               ' ' -> return
               c   -> const (Left $ "Dont know how to parse " <> (T.pack . show) c)
      in
        op (tm, carts)

    addCart pos dir (tm, carts) = return (tm, Cart pos dir LeftTurn : carts)
    addElt pos elt (tm, carts) = return (M.insert pos elt tm, carts)

coordChars :: Text -> [(Pos, Char)]
coordChars s = 
 flip concatMap (zip [0..] (T.lines s)) $ \(y, line) ->
    flip map (zip [0..] (T.unpack line)) $ \(x, c) ->
      (Pos x y, c)

mapSize :: TrackMap -> (Int, Int)
mapSize tm =
  let
    positions = M.keys tm
  in
    (safeMax (_x <$> positions) + 1, safeMax (_y <$> positions) + 1)
  where
    safeMax xs = (getMax . sconcat) $ Max 0 :| map Max xs

-- debugRender = T.putStrLn $ uncurry renderGame sample

nextTurn :: Turn -> Turn
nextTurn LeftTurn = NoTurn
nextTurn NoTurn = RightTurn
nextTurn RightTurn = NoTurn

moveInDir :: Dir -> Pos -> Pos
moveInDir N (Pos x y) = Pos x (y - 1)
moveInDir E (Pos x y) = Pos (x + 1) y
moveInDir S (Pos x y) = Pos x (y + 1)
moveInDir W (Pos x y) = Pos (x - 1) y

turnDir :: Turn -> Dir -> Dir
turnDir LeftTurn N = W
turnDir LeftTurn E = N
turnDir LeftTurn S = E
turnDir LeftTurn W = S
turnDir RightTurn N = E
turnDir RightTurn E = S
turnDir RightTurn S = W
turnDir RightTurn W = N
turnDir _ x = x

moveCart :: TrackMap -> Cart -> Cart
moveCart tm (Cart pos dir turn) =
  let
    pos' = moveInDir dir pos
    (dir', turn') = maybe (dir, turn) (f dir) (M.lookup pos' tm)
  in
    Cart pos' dir' turn'

  where
    f _ Hor = (dir, turn)
    f _ Vert = (dir, turn)

    f N Slash = (E, turn)
    f E Slash = (N, turn)
    f S Slash = (W, turn)
    f W Slash = (S, turn)

    f N BSlash = (W, turn)
    f E BSlash = (S, turn)
    f S BSlash = (E, turn)
    f W BSlash = (N, turn)

    f dir Cross = (turnDir turn dir, nextTurn turn)

moveChecked
  :: TrackMap
  -> Map Pos Cart
  -> Cart
  -> Either Pos (Map Pos Cart)
moveChecked tm m cart =
  let
    m2 = M.delete (_cartPos cart) m
    cart2 = moveCart tm cart
  in
    if M.member (_cartPos cart2) m2
      then Left (_cartPos cart2)
      else Right (M.insert (_cartPos cart2) cart2 m2)

indexCarts :: [Cart] -> Map Pos Cart
indexCarts carts = M.fromList $ fmap (_cartPos &&& id) carts

data State
  = State
    { _stateTm :: TrackMap
    , _stateMap :: Map Pos Cart
    , _stateCarts :: [Cart]
    , _stateCrash :: Maybe Pos
    }

initState :: TrackMap -> [Cart] -> State
initState tm carts = State tm (indexCarts carts) [] Nothing


step :: State -> State
step s@(State _ _ _ (Just _)) = s

step (State tm m [] mc) = 
  step (State tm m (sortOn _cartPos (M.elems m)) mc)

step s@(State tm m (cart:xs) _) = 
  either
    (\pos -> s { _stateCrash = Just pos } )
    (\m2 -> s { _stateMap = m2, _stateCarts = xs } )
    (moveChecked tm m cart)

renderGame :: State -> Text
renderGame (State tm m _ mc) =
  let
    carts = M.elems m
    (width, height) = mapSize tm
    cartByPos = M.fromList $ (_cartPos &&& id) <$> carts
  in
    (T.unlines . fmap T.pack) $
      flip fmap [0..height-1] (\y ->
        (flip concatMap [0..width-1] $ \x ->
          fromMaybe " " $ asum [ mc >>= h (Pos x y)
                               , g <$> M.lookup (Pos x y) cartByPos
                               , f <$> M.lookup (Pos x y) tm]
        )
      )
      ++ status mc
  where
    f Hor = "-"
    f Vert = "|"
    f Slash = "/"
    f BSlash = "\\"
    f Cross = "+"

    g (_cartDir -> dir) =
      case dir of
        N -> "^"
        E -> ">"
        S -> "v"
        W -> "<"

    h pos c
      | c == pos  = Just "X"
      | otherwise = Nothing

    status Nothing = [""]
    status (Just pos) = ["Crash at " ++ show pos]

    
simulate :: State -> IO ()
simulate state = do
  T.putStrLn (renderGame state)
  getLine
  simulate (step state)


untilCrash :: State -> Pos
untilCrash (State _ _ _ (Just pos)) = pos
untilCrash state = untilCrash (step state)

--------------------------------------------------------------------------------

sampleInput :: Text
sampleInput = T.strip [r|
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
|]

sample = fromRight (error "whoops") $ parseGame sampleInput

s1 = uncurry initState sample
  
sol1 = runPuzzle solution "day13" Part1 

parseForce :: Text -> State
parseForce = uncurry initState . fromRight (error "parsing") . parseGame

solution :: PuzzlePart -> Text -> Text
solution Part1 input =
    T.pack . show $ untilCrash (parseForce input)

input1 :: IO Text
input1 = T.readFile ("inputs/" <> "day13" <> ".txt")


{-# LANGUAGE ScopedTypeVariables #-}

module Day13 where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List (foldl', sortBy, tails)
import Data.Maybe (catMaybes)
import Control.Monad (when)
import Control.Monad.Loops (iterateUntilM)

--
-- Segments of the track
--

data Segment
  = Horizontal -- '-'
  | Vertical -- '|'
  | LeftDown -- '\'
  | RightUp-- '/'
  | Intersection -- '+'
  | Empty -- ' '
  deriving Show

toChar :: Segment -> Char
toChar Horizontal = '-'
toChar Vertical = '|'
toChar LeftDown = '\\'
toChar RightUp = '/'
toChar Intersection = '+'
toChar Empty = ' '

fromChar :: Char -> Segment
fromChar '-' = Horizontal
fromChar '|' = Vertical
fromChar '\\' = LeftDown
fromChar '/' = RightUp
fromChar '+' = Intersection
fromChar ' ' = Empty
fromChar _ = error "Unknown segment char"

--
-- Track network, coordinates are (x, y) where x runs from 0 left to right, and y from 0 top to bottom
--

newtype Network = Network (Array (Int, Int) Segment)

instance Show Network where
  show (Network n) = init $ unlines [ row y | y <- [0..ymax]]
    where
      ((0, 0), (xmax, ymax)) = A.bounds n
      row :: Int -> String
      row j = [ toChar (n A.! (x, j)) | x <- [0..xmax]]

--
-- State for each cart
--
data Cart = Cart
  { x :: Int
  , y :: Int
  , heading :: Direction
  , next :: Choice
}
instance Show Cart where
  show c = "(" ++ show (x c) ++ ", " ++ show (y c) ++ ") " ++ [head (show (heading c))] ++ show (next c)

-- Cart's current direction
data Direction = North | East | South | West deriving Show

-- Cart's choice at each intersection follows a sequence
data Choice = TurnLeft | GoStraight | TurnRight

instance Show Choice where
  show TurnLeft = "L"
  show TurnRight = "R"
  show _ = "-"

iterateChoice :: Choice -> Choice
iterateChoice TurnLeft = GoStraight
iterateChoice GoStraight = TurnRight
iterateChoice TurnRight = TurnLeft

--
-- Overall state of system
--

data State = State
  { network :: Network
  , carts :: [Cart]
  } deriving Show

testNetworkStr :: [String]
testNetworkStr =
  [ "/->-\\        "
  , "|   |  /----\\"
  , "| /-+--+-\\  |"
  , "| | |  | v  |"
  , "\\-+-/  \\-+--/"
  , "  \\------/   "
  ]

testNetwork2 :: State
testNetwork2 = readNetwork
  [ "/>-<\\  "
  , "|   |  "
  , "| /<+-\\"
  , "| | | v"
  , "\\>+</ |"
  , "  |   ^"
  , "  \\<->/"
  ]

-- | Read the initial state
--
-- >>> map show . carts $ readNetwork testNetworkStr
-- ["(2, 0) EL","(9, 3) SL"]
-- >>> network $ readNetwork testNetworkStr
-- /---\
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \-+--/
--   \------/
readNetwork :: [String] -> State
readNetwork xs = State
  { network = Network $ A.array ((0, 0), (xmax, ymax)) trackData
  , carts = map mkCart cartData
  }
  where
    xmax = length (head xs) - 1
    ymax = length xs - 1
    rowData :: [(Int, ([(Int, Segment)], [(Int, Direction)]))]
    rowData = zip [0..] $ map readRow xs
    trackData :: [((Int, Int), Segment)]
    trackData = concatMap unpackTrackRow rowData
    unpackTrackRow :: (Int, ([(Int, Segment)], a)) -> [((Int, Int), Segment)]
    unpackTrackRow (y, (xs, _)) = map (\(x, s) -> ((x, y), s)) xs
    cartData :: [(Int, Int, Direction)]
    cartData = concatMap unpackCartRow rowData
    unpackCartRow :: (Int, (b, [(Int, Direction)])) -> [(Int, Int, Direction)]
    unpackCartRow (y, (_, xs)) = map (\(x, d) -> (x, y, d)) xs
    mkCart :: (Int, Int, Direction) -> Cart
    mkCart (x, y, d) = Cart { x = x, y = y, heading = d, next = TurnLeft }

-- | Read one row
--
-- >>> map fst . fst . readRow $ "|   |  /----\\"
-- [0,1,2,3,4,5,6,7,8,9,10,11,12]
-- >>> map (toChar . snd) . fst . readRow $ "|   |  /----\\"
-- "|   |  /----\\"
-- >>> snd . readRow $ "|   |  /----\\"
-- []
-- >>> map fst . fst . readRow $ "/->-\\        "
-- [0,1,2,3,4,5,6,7,8,9,10,11,12]
-- >>> map (toChar . snd) . fst . readRow $ "/->-\\        "
-- "/---\\        "
-- >>> snd . readRow $ "/->-\\        "
-- [(2,East)]
-- >>> snd . readRow $ "| | |  | v  |"
-- [(9,South)]
readRow :: String -> ([(Int, Segment)], [(Int, Direction)])
readRow s = (zip [0..] (map readTrack s), cartIndices s)
  where
    readTrack :: Char -> Segment
    readTrack '>' = Horizontal
    readTrack '<' = Horizontal
    readTrack '^' = Vertical
    readTrack 'v' = Vertical
    readTrack c = fromChar c
    cartIndices :: String -> [(Int, Direction)]
    cartIndices = catMaybes . zipWith readCart [0..]
    readCart :: Int -> Char -> Maybe (Int, Direction)
    readCart i '>' = Just (i, East)
    readCart i 'v' = Just (i, South)
    readCart i '<' = Just (i, West)
    readCart i '^' = Just (i, North)
    readCart _ _ = Nothing

-- | Run system until collision, return collision coordinates
--
-- >>> runToCollision $ readNetwork testNetworkStr
-- (7,3)
runToCollision :: State -> (Int, Int)
runToCollision s = case tick s of
  Left pos -> pos
  Right s' -> runToCollision s'

runLog :: State -> IO ()
runLog state = do
  showState 0 state
  go 0 state
    where
      go i s = case tick s of
        Left pos -> print pos
        Right s' -> do
          showState (i + 1) s'
          go (i + 1) s'
      showState i t = putStrLn $ show i ++ ": " ++ show (map (\c -> (x c, y c)) (sortCarts (carts t)))

-- Run system new state or coordinate of first collision
tick :: State -> Either (Int, Int) State
tick s = fmap (\cs -> s { carts = cs }) newCarts
  where
    n :: Network = network s
    sortedCarts :: [Cart] = sortCarts $ carts s
    newCarts :: Either (Int, Int) [Cart] = foldl' f (Right []) (tails sortedCarts)
    f :: Either (Int, Int) [Cart] -> [Cart] -> Either (Int, Int) [Cart]
    f (Left pos) _ = Left pos
    f (Right movedCarts) [] = Right movedCarts
    f (Right movedCarts) (c : unmovedCarts)
      | collides c' (movedCarts ++ unmovedCarts) = Left (x c', y c')
      | otherwise = Right (c' : movedCarts)
      where
        c' = updateCart n c

-- | Run system to coordinate of last surviving cart
--
-- >>> runToLastCart False testNetwork2
-- (6,4)
runToLastCart :: Bool -> State -> IO (Int, Int)
runToLastCart log s = do
  (lastCartsMoved, lastCartsUnmoved) <- iterateUntilM oneCart moveCart ([], sortCarts (carts s))
  let lastCart = head (lastCartsMoved ++ map (updateCart n) lastCartsUnmoved) -- Finish tick
  return (x lastCart, y lastCart)
  where
    n :: Network = network s
    moveCart :: ([Cart], [Cart]) -> IO ([Cart], [Cart])
    moveCart (movedCarts, unmovedCarts@(c:rest)) = do
      when log $ print (movedCarts, unmovedCarts)
      return (movedCarts', rest')
      where
        c' = updateCart n c
        movedCarts'
          | collides c' (movedCarts ++ rest) = filter (differentPosition c') movedCarts
          | otherwise = c' : filter (differentPosition c') movedCarts
        rest' = filter (differentPosition c') rest
        numCarts' = length movedCarts' + length rest'
    moveCart (movedCarts, []) = moveCart ([], sortCarts movedCarts)
    oneCart (movedCarts, unmovedCarts) = (length movedCarts + length unmovedCarts) <= 1
    differentPosition a b = x a /= x b || y a /= y b

sortCarts :: [Cart] -> [Cart]
sortCarts = sortBy cmpCarts
  where
    cmpCarts :: Cart -> Cart -> Ordering
    cmpCarts c1 c2 = case compare (y c1) (y c2) of
      LT -> LT
      GT -> GT
      EQ -> compare (x c1) (x c2)

-- Does the cart have same (x, y) as any of the carts in the list
collides :: Cart -> [Cart] -> Bool
collides c = any (sameCoord c)
  where sameCoord c1 c2 = x c1 == x c2 && y c1 == y c2

-- | Update position of cart with one move
--
-- >>> take 8 $ iterate (updateCart (network (readNetwork testNetworkStr))) (Cart { x = 2, y = 0, heading = East, next = TurnLeft })
-- [(2, 0) EL,(3, 0) EL,(4, 0) EL,(4, 1) SL,(4, 2) SL,(5, 2) E-,(6, 2) E-,(7, 2) E-]
updateCart :: Network -> Cart -> Cart
updateCart (Network n) c = case (n A.! (x c, y c), heading c) of
  -- Horizontal
  (Horizontal, East) -> c_east
  (Horizontal, West) -> c_west
  -- Vertical
  (Vertical, South) -> c_south
  (Vertical, North) -> c_north
  --  LeftDown -- '\'
  (LeftDown, East) -> c_south
  (LeftDown, North) -> c_west
  (LeftDown, West) -> c_north
  (LeftDown, South) -> c_east
  --  | RightUp-- '/'
  (RightUp, East) -> c_north
  (RightUp, North) -> c_east
  (RightUp, West) -> c_south
  (RightUp, South) -> c_west
  -- Intersection
  (Intersection, East) -> (case next c of
    TurnLeft -> c_north
    TurnRight -> c_south
    _ -> c_east) { next = iterateChoice (next c) }
  (Intersection, West) -> (case next c of
    TurnLeft -> c_south
    TurnRight -> c_north
    _ -> c_west) { next = iterateChoice (next c) }
  (Intersection, North) -> (case next c of
    TurnLeft -> c_west
    TurnRight -> c_east
    _ -> c_north) { next = iterateChoice (next c) }
  (Intersection, South) -> (case next c of
    TurnLeft -> c_east
    TurnRight -> c_west
    _ -> c_south) { next = iterateChoice (next c) }
  (s, d) -> error $ "Bad update" ++ show s ++ " " ++ show d
  where
    c_north = c { y = y c - 1, heading = North }
    c_south = c { y = y c + 1, heading = South }
    c_west = c { x = x c - 1, heading = West }
    c_east = c { x = x c + 1, heading = East }

main :: IO ()
main = do
  input <- readFile "input/day13.txt"
  let initialState = readNetwork $ lines input
  putStrLn $ "day 13 part a: " ++ show (runToCollision initialState)
  putStr "day 13 part b: "
  lastCoords <- runToLastCart False initialState
  print lastCoords

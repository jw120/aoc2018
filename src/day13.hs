{-# LANGUAGE ScopedTypeVariables #-}

module Day13 where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Maybe (catMaybes)

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

data Network = Network (Array (Int, Int) Segment)

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

move :: Direction -> (Int, Int) -> (Int, Int)
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x + 1, y)

-- Cart's choice at each intersection follows a sequence
data Choice = TurnLeft | StraightAfterLeft | TurnRight | StraightAfterRight

instance Show Choice where
  show TurnLeft = "L"
  show TurnRight = "R"
  show _ = "-"

iterateChoice :: Choice -> Choice
iterateChoice TurnLeft = StraightAfterLeft
iterateChoice StraightAfterLeft = TurnRight
iterateChoice TurnRight = StraightAfterRight
iterateChoice StraightAfterRight = TurnLeft

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

-- | Read the initial state
--
-- >>> map show . carts $ readNetwork testNetworkStr
-- ["(2, 0) E-","(9, 3) S-"]
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
    xmax = (length (head xs)) - 1
    ymax = (length xs) - 1
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
    mkCart (x, y, d) = Cart { x = x, y = y, heading = d, next = StraightAfterRight }

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
    cartIndices = catMaybes . map readCart . zip [0..]
    readCart :: (Int, Char) -> Maybe (Int, Direction)
    readCart (i, '>') = Just (i, East)
    readCart (i, 'v') = Just (i, South)
    readCart (i, '<') = Just (i, West)
    readCart (i, '^') = Just (i, North)
    readCart _ = Nothing

-- Run system new state or coordinate of first collision
tick :: State -> Either (Int, Int) State
tick = undefined

main :: IO ()
main = do
  input <- readFile "input/day13.txt"
  let initialState = readNetwork $ lines input
  putStrLn $ "day 13 part a: " ++ "NYI"
  putStrLn $ "day 13 part b: " ++ "NYI"

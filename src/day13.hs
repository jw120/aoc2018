{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day13 where

import Data.Array (Array)
import qualified Data.Array as A

-- Segments of the track
data Segment
  = Horizontal -- '-'
  | Vertical -- '|'
  | LeftDown -- '\'
  | RightUp-- '/'
  | Intersection -- '+'
  | Empty -- ' '

toChar :: Segment -> Char
toChar Horizontal = '-'
toChar Vertical = '|'
toChar LeftDown = '\\'
toChar RightUp = '/'
toChar Intersection = '+'
toChar Empty = ' '

fromChar :: Char -> Segment
fromChar ' ' = Horizontal
fromChar ' ' = Vertical
fromChar ' ' = LeftDown
fromChar ' ' = RightUp
fromChar ' ' = Intersection
fromChar ' ' = Empty
fromChar _ = error "Unknown segment char"

-- Track network, coordinates are (x, y) where x runs from 0 left to right, and y from 0 top to bottom
data Network = Network (Array (Int, Int) Segment)

instance Show Network where
  show (Network n) = unlines [ row y | y <- [0..ymax]]
    where
      ((0, xmax), (0, ymax)) = A.bounds n
      row :: Int -> String
      row j = [ toChar (n A.! (x, j)) | x <- [0..xmax]]

-- Cart's choice at each intersection follows a sequence

data Choice = TurnLeft | StraightAfterLeft | TurnRight | StraightAfterRight

iterateChoice :: Choice -> Choice
iterateChoice TurnLeft = StraightAfterLeft
iterateChoice StraightAfterLeft = TurnRight
iterateChoice TurnRight = StraightAfterRight
iterateChoice StraightAfterRight = TurnLeft

-- State for each cart
data Cart = Cart
  { x :: Int
  , y :: Int
  , next :: Choice
}

-- Overall state of system
data State = State
  { network :: Network
  , carts :: [Cart]
  }

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
-- >>> network . readNetwork $ map BC.pack testNetworkStr
-- /---\
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \-+--/
--  \------/
-- >>> carts . readNetwork $ map BC.pack testNetworkStr
-- []
readNetwork :: [String] -> State
readNetwork xs = State
  { network = Network $ A.array ((0, xmax), (0, ymax)) undefined
  , carts = undefined
  }
  where
    xmax = (length xs) - 1
    ymax = (length (head xs)) - 1
    mapWithinRow :: String -> [(Int, Segment)]
    mapWithinRow = zip [0..] . map fromChar
    mapAcrossRows :: [(Int, Segment)] -> [((Int, Int), Segment)]
    mapAcrossRows = zipWith (\x (y, s) -> ((x, y), s)) [0..]


main :: IO ()
main = do
  input <- readFile "input/day13.txt"
  let initialState = readNetwork $ lines input
  putStrLn $ "day 13 part a: " ++ "NYI"
  putStrLn $ "day 13 part b: " ++ "NYI"

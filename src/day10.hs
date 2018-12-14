{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day10 where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

import qualified Data.Map as M
import Data.Map (Map)

data Point = Point
  { position :: (Int, Int)
  , velocity :: (Int, Int)
  } deriving Show

-- | Read a point
--
-- >>> readPoint $ BC.pack "position=<-6, 10> velocity=< 2, -2>"
-- Point {position = (-6,10), velocity = (2,-2)}
readPoint :: ByteString -> Point
readPoint = either error id . AC.parseOnly point
  where
    point :: AC.Parser Point
    point = do
      AC.string "position=<"
      AC.skipSpace
      x <- AC.signed AC.decimal
      AC.string ","
      AC.skipSpace
      y <- AC.signed AC.decimal
      AC.string "> velocity=<"
      AC.skipSpace
      vx <- AC.signed AC.decimal
      AC.string ","
      AC.skipSpace
      vy <- AC.signed AC.decimal
      AC.string ">"
      return Point { position = (x, y), velocity = (vx, vy) }

testData :: [Point]
testData = map readPoint
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]

boundingBox :: (Ord a, Ord b) => [(a, b)] -> ((a, b), (a, b))
boundingBox xs = ((xMin, yMin), (xMax, yMax))
  where
    xMin = minimum $ map fst xs
    xMax = maximum $ map fst xs
    yMin = minimum $ map snd xs
    yMax = maximum $ map snd xs

-- | Convert points into a grid of #s and .s suitable for printing to console
--
-- >>> head $ showPoints testData
-- "........#............."
-- >>> (showPoints testData) !! 2
-- ".........#.#..#......."
showPoints :: [Point] -> [String]
showPoints points = [showRow r | r <- [yMin..yMax]]
  where
    positions :: [(Int, Int)] = map position points
    ((xMin, yMin), (xMax, yMax)) = boundingBox positions
    showRow :: Int -> String
    showRow r = [if (i, r) `elem` positions then '#' else '.' | i <- [xMin .. xMax]]

-- | Advance a point by its velocity
--
-- >>> advance Point { position = (3,9), velocity = (1, -2)}
-- Point {position = (4,7), velocity = (1,-2)}
advance :: Point -> Point
advance p = p { position = (x + vx, y + vy) }
  where (x, y) = position p
        (vx, vy) = velocity p

-- | Area of the bounding box of a set of points
area :: [Point] -> Int
area points = (xMax - xMin) * (yMax - yMin)
  where
    positions :: [(Int, Int)] = map position points
    ((xMin, yMin), (xMax, yMax)) = boundingBox positions

run :: [Point] -> IO ()
run points = putStr . unlines $ "Day 10 " : show minIndex : showPoints minBoard
  where
    allBoards :: [[Point]] = iterate (map advance) points
    allBoards' :: [(Int, (Int, [Point]))] = zipWith (\b i -> (area b, (i, b))) allBoards [0..]
    (minIndex, minBoard) :: (Int, [Point]) = firstLocalMin allBoards'

-- | Find first local minimum of an indexed list
--
-- >>> firstLocalMin [(3, 'A'),(2, 'B'),(1, 'C'),(2, 'D'),(0, 'E')]
-- 'C'
firstLocalMin :: Ord x => [(x, y)] -> y
firstLocalMin ((x1, y1) : (x2, y2) : (x3, y3) : rest)
    | x1 > x2 && x2 < x3 = y2
    | otherwise = firstLocalMin ((x2, y2) : (x3, y3) : rest)
firstLocalMin _ = error "Cannot find local minimum"

main :: IO ()
main = do
  input <- B.readFile "input/day10.txt"
  run . map readPoint $ BC.lines input

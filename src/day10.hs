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

-- | Convert points into a grid of #s and .s suitable for printing to console
--
-- >>> head $ showPoints testData
-- "........#............."
-- >>> (showPoints testData) !! 2
-- ".........#.#..#......."
showPoints :: [Point] -> [String]
showPoints points = header : [showRow r | r <- [yMin..yMax]]
  where
    header = show xMin ++ ".." ++ show xMax ++ ", " ++ show yMin ++ ", " ++ show yMax
    positions :: [(Int, Int)] = map position points
    xMin = minimum $ map fst positions
    xMax = maximum $ map fst positions
    yMin = minimum $ map snd positions
    yMax = maximum $ map snd positions
    showRow :: Int -> String
    showRow r = [if (i, r) `elem` positions then '#' else '.' | i <- [xMin .. xMax]]

-- | Advance a point by its velocity
--
-- >>> advance Point { position = (3,9), velocity = (1, -2)}
-- Point {position=(4,-7), velocity=(1,-2)}
advance :: Point -> Point
advance p = p { position = (x + vx, y + vy) }
  where (x, y) = position p
        (vx, vy) = velocity p

area :: [Point] -> Int
area points = (xMax - xMin) * (yMax - yMin)
  where
    positions :: [(Int, Int)] = map position points
    xMin = minimum $ map fst positions
    xMax = maximum $ map fst positions
    yMin = minimum $ map snd positions
    yMax = maximum $ map snd positions

partA :: Int -> [Point] -> IO ()
partA lastTick points = go 0 points
  where
    go :: Int -> [Point] -> IO ()
    go i ps
      | i == lastTick = printBoard
      | otherwise = do
          printBoard
          go (i + 1) (map advance ps)
      where
        printBoard = do
          putStrLn $ "After " ++ show i ++ " seconds"
          putStr . unlines $ showPoints ps
          putStrLn ""

partA' :: [Point] -> IO ()
partA' = go 0
  where
    go :: Int -> [Point] -> IO ()
    go lastArea points
      | lastArea == 0 || a < lastArea = go a (map advance points)
      | otherwise = putStr . unlines $ showPoints points
      where
        a = area points

partA'' :: [Point] -> IO ()
partA'' points = mapM_ printBoard smallBoards
  where
    allBoards :: [[Point]] = iterate (map advance) points
    smallBoards :: [[Point]] = filter ((<= maxArea) . area) allBoards
    maxArea =  (226 - 156 + 2) * (125 - 106 + 2)
    printBoard :: [Point] -> IO ()
    printBoard = putStr . unlines . showPoints

main :: IO ()
main = do
  input <- B.readFile "input/day10.txt"
  let points = map readPoint $ BC.lines input
  putStrLn $ "day 10 part a: "
  partA'' points
  putStrLn $ "day 10 part b: " ++ "NYI"



{-# LANGUAGE ScopedTypeVariables #-}

module Day11 where

import Data.Array (Array, array, assocs, (!))
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromJust)

-- Problem size
n :: Int
n = 300

-- | Extract the hundreds digit from a positive integer
--
-- >>> map hundredsDigit [1, 21, 342, 4562, 76523]
-- [0,0,3,5,5]
hundredsDigit :: Int -> Int
hundredsDigit x = ((x - x `mod` 100) `div` 100) `mod` 10

-- | Compute power level of a cell
--
-- >>> powerLevel 8 3 5
-- 4
-- >>> powerLevel 57 122 79
-- -5
-- >>> powerLevel 39 217 196
-- 0
-- >>> powerLevel 71 101 153
-- 4
powerLevel :: Int -> Int -> Int -> Int
powerLevel serial x y = hundredsDigit (initialPowerLevel * rackID) - 5
  where
    rackID = x + 10
    initialPowerLevel = rackID * y + serial

-- | Part a , return the top-left coordinates of the 3x3 square with the highest powerLevel
--
-- >>> highestSquare 18
-- ((33,45),29)
-- >>> highestSquare 42
-- ((21,61),30)
highestSquare :: Int -> ((Int, Int), Int)
highestSquare serial = maximumBy (compare `on` snd) boxScores
  where
    g :: Array (Int, Int) Int =
      array ((1, 1), (n, n)) [((x, y), powerLevel serial x y) | x <- [1..n], y <- [1..n]]
    boxScores :: [((Int, Int), Int)] =
      [((x, y), box x y) | x <- [1..n-2], y <- [1..n-2]]
    box :: Int -> Int -> Int
    box x y = sum [g ! (x + i, y + j) | i <- [0..2], j <- [0..2]]

-- | Part b, Return the top-left coordinates and size of square with the highest powerLevel
highestSquareOfAnySize :: Int -> ((Int, Int, Int), Int)
highestSquareOfAnySize serial = maximumBy (compare `on` snd) . filterJusts $ assocs boxScores
    where
      -- Scores of single squares
      g :: Array (Int, Int) Int =
        array ((1, 1), (n, n)) [((x, y), powerLevel serial x y) | x <- [1..n], y <- [1..n]]
      -- Scores of squares of given size (constructed lazilly), nothing when size is out of bounds
      boxScores :: Array (Int, Int, Int) (Maybe Int) = array ((1, 1, 1), (n, n, n))
        [((x, y, s), box s x y) | s <- [1 .. n], x <- [1 .. n], y <- [1 .. n]]
      box :: Int -> Int -> Int -> Maybe Int
      box s x y
        | s == 1 = Just (g ! (x, y))
        | x + s > n || y + s > n = Nothing
        | otherwise = Just $
            fromJust (boxScores ! (x, y, s - 1)) +
            sum [g ! (i, y + s - 1) | i <- [x .. x + s - 1]] +
            sum [g ! (x + s - 1, j) | j <- [y .. y + s - 1]] -
            g ! (x + s - 1, y + s - 1)
      filterJusts :: [((Int, Int, Int), Maybe Int)] -> [((Int, Int, Int), Int)]
      filterJusts = map removeJust . filter hasJust
        where
          hasJust (_, Just _) = True
          hasJust (_, Nothing) = False
          removeJust (c, Just x) = (c, x)
          removeJust (_, Nothing) = error "Unexpected removeJust"

main :: IO ()
main = do
  putStrLn $ "day 11 part a: " ++ show (fst (highestSquare 9445))
  putStrLn $ "day 11 part b: " ++ show (fst (highestSquareOfAnySize 9445))

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Set as Set

-- | Read an integer from a string ignoring a leading '+'
--
-- >>> map readPlusMinus ["+23", "23", "-23"]
-- [23, 23, -23]
readPlusMinus :: String -> Int
readPlusMinus ('+' : rest) = read rest
readPlusMinus s = read s

-- | Return first repeated value in a (potentially infinite) list of Ints
--
-- >>> firstRepeat [1, 2, 3, 4, 2, 4, 5]
-- 2
firstRepeat :: [Int] -> Int
firstRepeat = firstRepeat' Set.empty
  where
    firstRepeat' :: Set.Set Int -> [Int] -> Int
    firstRepeat' visited (x : xs)
      | x `Set.member` visited = x
      | otherwise = firstRepeat' (x `Set.insert` visited ) xs
    firstRepeat' _ [] = error "Unexpected empty list in firstRepeat"

main :: IO ()
main = do
  input <- readFile "input/day01.txt"
  let changes = map readPlusMinus . lines $ input
  let sums = scanl (+) 0 $ cycle changes
  putStrLn $ "day 01 part a: " ++ show (sum changes)
  putStrLn $ "day 01 part b: " ++ show (firstRepeat sums)

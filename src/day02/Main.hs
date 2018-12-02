{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map as Map

-- Part a solution. Product of number
checkSum :: [String] -> Int
checkSum ids = count2 * count3
  where ( count2, count3) = foldl accPair (0, 0) $ map hasReps ids

-- | Accumulate counts of pairs of booleans, used for fold
--
-- >>> accPair (3, 4) (True, False)
-- (4, 4)
accPair :: (Int, Int) -> (Bool, Bool) -> (Int, Int)
accPair (acc1, acc2) (x1, x2) = (acc1 + if x1 then 1 else 0, acc2 + if x2 then 1 else 0)

-- | Does the string contain a doubly-repeated and/or a triply-repeat character
--
-- >>> hasReps "aabbcccc"
-- (True, False)
hasReps :: [Char] -> (Bool, Bool)
hasReps s = (2 `elem` counts, 3 `elem` counts)
  where
    countMap = buildCountMap s
    counts = Map.elems countMap

-- | Build a map of the counts of each element of the list
--
-- >>> Map.toList $ buildCountMap "abbccdedd"
-- [('a', 1), ('b', 2), ('c', 2), ('d', 3), ('e', 1)]
buildCountMap :: [Char] -> Map.Map Char Int
buildCountMap = foldl addCount Map.empty
    where addCount m x = Map.insertWith (+) x 1 m

main :: IO ()
main = do
  input <- readFile "input/day02.txt"
  let ids = lines input
  putStrLn $ "day 02 part a: " ++ show (checkSum ids)
  putStrLn $ "day 02 part b: NYI"

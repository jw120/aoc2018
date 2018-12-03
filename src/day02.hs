{-# LANGUAGE ScopedTypeVariables #-}

module Day02 where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Map (Map)

-- Part (a) solution. Product of number of ids with a 2-count character and the number
-- with a 3-count character
checkSum :: [String] -> Int
checkSum ids = count2 * count3
  where
    (count2, count3) = foldl' accPair (0, 0) $ map hasReps ids
    accPair :: (Int, Int) -> (Bool, Bool) -> (Int, Int)
    accPair (acc1, acc2) (x1, x2) = (acc1 + if x1 then 1 else 0, acc2 + if x2 then 1 else 0)

-- | Does the list contain a doubly-repeated and/or a triply-repeated element
--
-- >>> hasReps "aabbcccc"
-- (True,False)
hasReps :: Ord k => [k] -> (Bool, Bool)
hasReps s = (2 `elem` counts, 3 `elem` counts)
  where counts = Map.elems $ buildCountMap s

-- | Build a map of the counts of each element of the list
--
-- >>> Map.toList $ buildCountMap "abbccdedd"
-- [('a',1),('b',2),('c',2),('d',3),('e',1)]
buildCountMap :: Ord k => [k] -> Map k Int
buildCountMap = foldl' addCount Map.empty
    where addCount m x = Map.insertWith (+) x 1 m

-- Part (b) solution. Finds pair of ids which have only one differing character and
-- return the common characters
pairWithOneDiff :: [String] -> String
pairWithOneDiff ids = commonElements
    where
      commonElements = map (\(a, _) -> a) . filter (\(a, b) -> a == b) $ zip id1 id2
      (True, id1, id2) = head $ filter (\(d, _, _) -> d) labelledPairs
      labelledPairs = map (\(id1, id2) -> (hasOneDiff id1 id2, id1, id2)) pairs
      pairs = allPairs ids

-- | Generate all unordered pairs of elements from a list
--
-- >>> allPairs "abcd"
-- [('a','b'),('a','c'),('a','d'),('b','c'),('b','d'),('c','d')]
allPairs :: [a] -> [(a, a)]
allPairs (x: xs) = zip (repeat x) xs ++ allPairs xs
allPairs [] = []

-- | Are the two lists identical except for precisely one difference
--
-- >>> hasOneDiff "abcd" "abxd"
-- True
-- >>> hasOneDiff "xyz" "xyz"
-- False
-- >>> hasOneDiff "pqrs" "pQrS"
-- False
-- >>> hasOneDiff "abc" "abcd"
-- False
hasOneDiff :: Eq a => [a] -> [a] -> Bool
hasOneDiff = hasOneDiff' False
  where
    hasOneDiff' found (x:xs) (y:ys)
      | x == y = hasOneDiff' found xs ys -- Keep going if not different
      | found = False -- Fail if find second difference
      | otherwise = hasOneDiff' True xs ys -- Keep going with first difference
    hasOneDiff' found [] [] = found -- Succed if only one difference and both at end
    hasOneDiff' _ _ _ = False -- Fail if mismatched lists

main :: IO ()
main = do
  input <- readFile "input/day02.txt"
  let ids = lines input
  putStrLn $ "day 02 part a: " ++ show (checkSum ids)
  putStrLn $ "day 02 part b: " ++ pairWithOneDiff ids

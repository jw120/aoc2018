module Day08 where

import Control.Monad (unless)

testData :: [Int]
testData = map read $ words "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

data Node = Node [Node] [Int] deriving Show

-- | Simple parser for a node (no errors caught)
--
-- >>> node testData
-- (Node [Node [] [10,11,12],Node [Node [] [99]] [2]] [1,1,2],[])
node :: [Int] -> (Node, [Int])
node (numChildren : numMetadata : afterHeader) = (Node cs ms, afterMetadata)
  where
    (cs, afterChildren) = rep numChildren node afterHeader
    (ms, afterMetadata) = splitAt numMetadata afterChildren

-- turn a parser into a parer for a list of n entries
rep :: Int -> ([Int] -> (a, [Int])) -> ([Int] -> ([a], [Int]))
rep 0 _ xs = ([], xs)
rep n p xs
  | n < 0 = error "negative in rep"
  | otherwise = (a1 : aN, restN)
    where
      (a1, rest1) = p xs
      (aN, restN) = rep (n - 1) p rest1

-- | Part a answer - sum of meta data in a node
--
-- >>> sumMetadata . fst $ node testData
-- 138
sumMetadata :: Node -> Int
sumMetadata (Node cs ms)= sum ms + sum (map sumMetadata cs)

main :: IO ()
main = do
  input <- readFile "input/day08.txt"
  let (inputNode, rest) = node . map read $ words input
  unless (null rest) (print ("Parse failed with leftovers: " ++ show rest))
  putStrLn $ "day 08 part a: " ++ show (sumMetadata inputNode)
  putStrLn $ "day 08 part b: " ++ "NYI"

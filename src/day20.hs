{-# LANGUAGE ScopedTypeVariables #-}

module Day20 where

import Data.Set (Set)
import qualified Data.Set as S


type Position = (Int, Int)

data Direction = North | East | South | West
instance Show Direction where
  show North = "N"
  show South = "S"
  show East = "E"
  show West = "W"

step :: Direction -> Position ->Position
North `step` (x, y) = (x, y + 1)
South `step` (x, y) = (x, y - 1)
East `step` (x, y) = (x + 1, y)
West `step` (x, y) = (x - 1, y)

-- | Read a regex path and return a set of open doors
--
-- We index each grid square by its (left-right, down-up) coordinates with (0,0) as the starting
-- square. Then we hold the partially generated map as a set of doors which are known to be open
--
-- So the example: ENWWW
--
-- #?#?#?#?#
-- ?.|.|.|.?
-- #?#?#?#-#
--    ?X|.?
--    #?#?#
--
-- Is given by the Set containing ( 1,0), ( 2,1), ( 1,2), (-1,2), (-3, 2)
--
-- >>> S.toAscList $ readPath "ENWWW"
-- [(-3,2),(-1,2),(1,0),(1,2),(2,1)]
readPath :: String -> Set Position
readPath = fst . fst . go (S.empty, (0,0))
  where
      go :: (Set Position, Position) -> String -> ((Set Position, Position), String)
      go (s, p) "" = ((s, p), "")
      go (s, p) ('N' : rest) = go (addStep s p North) rest
      go (s, p) ('S' : rest) = go (addStep s p South) rest
      go (s, p) ('E' : rest) = go (addStep s p East) rest
      go (s, p) ('W' : rest) = go (addStep s p West) rest
      go (s, p) ('(' : rest) = ((mergedSets, dummyPosition), "")
        where
          alternativePaths = splitAlternatives rest
          mergedSets = S.unions $ map (fst. fst . go (s, p)) alternativePaths
          dummyPosition = (0, 0)
      go _ other = error $ "Unrecognized path: " ++ other
      addStep :: Set Position -> Position -> Direction -> (Set Position, Position)
      addStep s p d = (p1 `S.insert` s, p2)
        where
          p1 = d `step` p
          p2 = d `step` p1

-- | Split top level alternatives from a regex string
--
-- >>> splitAlternatives "E|SW|)NN"
-- ["ENN","SWNN","NN"]
-- >>> splitAlternatives "N(E|S)W|(N|S))(E|S)"
-- ["N(E|S)W(E|S)","(N|S)(E|S)"]
splitAlternatives :: String -> [String]
splitAlternatives s = map (\x -> x ++ remainder) splits
  where
    (splits, "", remainder) = go 0 ([], "", s)
    go :: Int -> ([String], String, String) -> ([String], String, String)
    go 0 (acc, current, '|' : rest) = go 0 (acc ++ [current], "", rest)
    go 0 (acc, current, ')' : rest) = (acc ++ [current], "", rest)
    go n (acc, current, ')' : rest) = go (n - 1) (acc, current ++ ")", rest)
    go n (acc, current, '(' : rest) = go (n + 1) (acc, current ++ "(", rest)
    go n (acc, current, c: rest) = go n (acc, current ++ [c], rest)

main :: IO ()
main = print "Day 20"
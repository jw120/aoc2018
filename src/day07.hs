{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day07 where

import Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
--import Data.Either (either)
--aimport Data.Function (on)
import Data.List (foldl', nub, sort)
import qualified Data.Map as M
import Data.Map (Map)
--import Data.Maybe (mapMaybe)

-- | Read a rule
--
-- >>> readRule $ BC.pack "Step C must be finished before step A can begin."
-- ('C','A')
readRule :: ByteString -> (Char, Char)
readRule = either error id . AC.parseOnly rule
  where
    rule :: AC.Parser (Char, Char)
    rule = do
      AC.string "Step "
      x <- AC.letter_ascii
      AC.string " must be finished before step "
      y <- AC.letter_ascii
      AC.string " can begin."
      return (x, y)

-- | Example rules from problem statement, ised for doctests
--
-- >>> testRules
-- [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
testRules :: [(Char, Char)]
testRules = map readRule
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

-- | Convert a list of rules into a map from chars to their direct prequisites
--
-- >>> M.toList $ toPrereqMap testRules
-- [('A',"C"),('B',"A"),('C',""),('D',"A"),('E',"BDF"),('F',"C")]
toPrereqMap :: [(Char, Char)] -> Map Char String
toPrereqMap rules = foldl' addRule allEmpty rules
  where
    addRule :: Map Char String -> (Char, Char) -> Map Char String
    addRule m (prereq, step) = M.insertWith (addSorted) step [prereq] m
    addSorted :: String -> String -> String
    addSorted s1 s2 = sort (s1 ++ s2)
    allSteps :: String
    allSteps = nub (map fst rules ++ map snd rules)
    allEmpty :: Map Char String
    allEmpty = M.fromList $ map (\c -> (c, "")) allSteps

main :: IO ()
main = do
  input <- B.readFile "input/day07.txt"
  let rules = map readRule $ BC.lines input
  putStrLn $ "day 06 part a: " ++ "NYI"
  putStrLn $ "day 06 part b: " ++ "NYI"

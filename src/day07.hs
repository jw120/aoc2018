{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}

module Day07 where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import qualified Data.Char as C
import Data.List (foldl', nub, sort)
import qualified Data.Map as M
import Data.Map (Map)

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
    addRule m (prereq, step) = M.insertWith addSorted step [prereq] m
    addSorted :: String -> String -> String
    addSorted s1 s2 = sort (s1 ++ s2)
    allSteps :: String
    allSteps = nub (map fst rules ++ map snd rules)
    allEmpty :: Map Char String
    allEmpty = M.fromList $ map (, "") allSteps

-- | What is the next step to be performed for given preqmap and already completed steps
--
-- >>> nextStep (toPrereqMap testRules) ""
-- Just 'C'
-- >>> nextStep (toPrereqMap testRules) "C"
-- Just 'A'
-- >>> nextStep (toPrereqMap testRules) "CA"
-- Just 'B'
-- >>> nextStep (toPrereqMap testRules) "ABCDEF"
-- Nothing
nextStep :: Map Char String -> String -> Maybe Char
nextStep prereqs done = case available of
  (c : _) -> Just c
  [] -> Nothing
  where
    incompleteSteps :: String = filter (`notElem` done) $ M.keys prereqs
    available :: String = filter prereqsDone incompleteSteps
    prereqsDone :: Char -> Bool
    prereqsDone step = all (`elem` done) $ prereqs M.! step

-- | What step should be performed for given preqmap
--
-- >>> allSteps testRules
-- "CABDFE"
allSteps :: [(Char, Char)] -> String
allSteps rules = allSteps' ""
  where
    prereqMap :: Map Char String = toPrereqMap rules
    allSteps' :: String -> String
    allSteps' done = case nextStep prereqMap done of
      Just c -> allSteps' (done  ++ [c])
      Nothing -> done

data ParallelState
  = Working [(Char, Int)] Int String
  | Finished
  deriving Show

-- | Advance one tick of parallel working
--
-- >>> parallelStep 2 0 (toPrereqMap testRules) (Working [] 0 "")
-- Working [('C',3)] 1 ""
-- >>> parallelStep 2 0 (toPrereqMap testRules) (Working [('C',3)] 1 "")
-- Working [('C',3)] 2 ""
-- >>> parallelStep 2 0 (toPrereqMap testRules) (Working [('C',3)] 2 "")
-- Working [('C',3)] 3 ""
-- >>> parallelStep 2 0 (toPrereqMap testRules) (Working [('C',3)] 3 "")
-- Working [('A',4),('F',9)] 4 "C"
parallelStep :: Int -> Int -> Map Char String -> ParallelState -> ParallelState
parallelStep _ _ _ Finished = error "Already finished"
parallelStep n baseTime prereqs (Working working tick done)
  | null nowIncomplete = Finished
  | otherwise = Working newWorking (tick + 1) nowDone
  where
    -- First finish the workers due to end this tick
    nowDone :: String = (done ++) . map fst $ filter ((== tick). snd) working
    nowIncomplete :: String = filter (`notElem` nowDone) $ M.keys prereqs
    nowWorking :: [(Char, Int)] = filter ((> tick) . snd) working
    -- Now start new workers
    nowAvailable :: String = sort . filter prereqsDone $ filter notInProgress nowIncomplete
    newStarts :: String = take (n - length nowWorking) nowAvailable
    newWorking :: [(Char, Int)]= nowWorking ++ map (addFinishTick tick) newStarts
    -- Helpers
    prereqsDone :: Char -> Bool
    prereqsDone step = all (`elem` nowDone) $ prereqs M.! step
    addFinishTick :: Int -> Char -> (Char, Int)
    addFinishTick t c = (c, C.ord(C.toLower c) - C.ord 'a' +  baseTime + 1 + t)
    notInProgress :: Char -> Bool
    notInProgress = (`notElem` map fst nowWorking)

-- | Run rules in parallel to completion
--
-- >>> allParallelSteps testRules 2 0
-- 15
allParallelSteps :: [(Char, Char)] -> Int -> Int -> Int
allParallelSteps rules n baseTime = go (Working [] 0 "")
    where
      prereqMap :: Map Char String = toPrereqMap rules
      go :: ParallelState -> Int
      go s@(Working _ tick _) = case parallelStep n baseTime prereqMap s of
        s'@Working{} -> go s'
        Finished -> tick

main :: IO ()
main = do
  input <- B.readFile "input/day07.txt"
  let rules = map readRule $ BC.lines input
  putStrLn $ "day 07 part a: " ++ allSteps rules
  putStrLn $ "day 07 part b: " ++ show (allParallelSteps rules 5 60)

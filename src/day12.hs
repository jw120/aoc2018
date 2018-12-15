{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day12 where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

newtype State = State [Bool]
data Rule = Rule Bool Bool Bool Bool Bool deriving Show
data Chunk = Chunk Bool Bool Bool Bool Bool deriving Show

-- | Output a state
--
-- >>> showState $ State [True, False, True]
-- "#.#"
showState :: State -> String
showState (State s)= map (\b -> if b then '#' else '.') s

-- | Read the initial state
--
-- >>> showState . readInitial $ BC.pack "initial state: #..#.#..##......###...###"
-- "#..#.#..##......###...###"
readInitial :: ByteString -> State
readInitial = either error id . AC.parseOnly initial
  where
    initial :: AC.Parser State
    initial = undefined

-- | Read a rule
--
-- >>> readRule $ BC.pack "#.### => #"
-- Rule True False True True True
readRule :: ByteString -> Rule
readRule = either error id . AC.parseOnly rule
  where
    rule :: AC.Parser Rule
    rule = undefined

testInitialState :: State
testInitialState = readInitial $ BC.pack "initial state: #..#.#..##......###...###"

testRules :: [Rule]
testRules = map readRule
  [ "...## => #"
  , "..#.. => #"
  , ".#... => #"
  , ".#.#. => #"
  , ".#.## => #"
  , ".##.. => #"
  , ".#### => #"
  , "#.#.# => #"
  , "#.### => #"
  , "##.#. => #"
  , "##.## => #"
  , "###.. => #"
  , "###.# => #"
  , "####. => #"
  ]

-- | Iterate rules once
--
apply :: [Rule] -> State -> State
apply rules s = State . map anyRuleMatches $ byChunk s
  where
    anyRuleMatches :: Chunk -> Bool
    anyRuleMatches c = any (\r -> ruleMatches r c) rules
    ruleMatches :: Rule -> Chunk -> Bool
    ruleMatches (Rule a b c d e) (Chunk p q r s t)
      = a == p && b == q && c == r && d == s && e == t

byChunk :: State -> [Chunk]
byChunk = undefined

-- | Iterate rules givem number of times
run :: Int -> [Rule] -> State -> State
run = undefined

-- | Sum of number of trues in a State
--
-- >>> sumState $ State [True, False, True, False, False]
-- 2
sumState :: State -> Int
sumState (State s) = length $ filter id s

main :: IO ()
main = do
  input <- B.readFile "input/day12.txt"
  let (header : rest) = BC.lines input
  let initialState = readInitial header
  let rules = map readRule $ filter (not . BC.null) rest
  putStrLn $ "day 12 part a: " ++ show (sumState (run 20 rules initialState))
  putStrLn $ "day 12 part b: " ++ "NYI"

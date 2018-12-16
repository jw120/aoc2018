{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day12 where

import Control.Applicative (many, (<|>))
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.Functor (($>))

newtype State = State [(Int, Bool)]
data Rule = Rule Bool Bool Bool Bool Bool deriving Show
data Chunk = Chunk Int Bool Bool Bool Bool Bool

instance Show Chunk where
  show (Chunk i a b c d e) = show i ++ ": " ++ map (\b -> if b then '#' else '.') [a, b, c, d, e]

instance Show State where
    show (State s) = show (fst (head s)) ++ ": " ++ map ((\b -> if b then '#' else '.') . snd) s

-- | Read the initial state
--
-- >>> readInitial $ BC.pack "initial state: #..#.#..##......###...###"
-- 0: #..#.#..##......###...###
readInitial :: ByteString -> State
readInitial = either error id . AC.parseOnly initial
  where
    initial :: AC.Parser State
    initial = State . zip [0..] <$> (AC.string "initial state: " *> many plant)


-- Parse a plant character
plant :: AC.Parser Bool
plant = (AC.char '#' $> True) <|> (AC.char '.' $> False)

-- | Read a rule
--
-- >>> readRule $ BC.pack "#.### => #"
-- Rule True False True True True
readRule :: ByteString -> Rule
readRule = either error id . AC.parseOnly rule
  where
    rule :: AC.Parser Rule
    rule = toRule <$> (many plant <* AC.string " => #")
      where
        toRule [a, b, c, d, e] = Rule a b c d e
        toRule _ = error "Bad rule"

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
-- >>> apply testRules testInitialState
-- 0: #...#....#.....#..#..#..#
apply :: [Rule] -> State -> State
apply rules s = State . trimEnds . map anyRuleMatches $ byChunk s
  where
    anyRuleMatches :: Chunk -> (Int, Bool)
    anyRuleMatches c@(Chunk i _ _ _ _ _) = (i, any (`ruleMatches` c) rules)
    ruleMatches :: Rule -> Chunk -> Bool
    ruleMatches (Rule a b c d e) (Chunk _ p q r s t)
      = a == p && b == q && c == r && d == s && e == t

-- | Remove False's from front and back of a list
--
-- >>> trimEnds $ zip [0..] [False, False, True, False, True, False]
-- [(2,True),(3,False),(4,True)]
trimEnds :: [(Int, Bool)] -> [(Int, Bool)]
trimEnds = dropWhile (not . snd) . reverse . dropWhile (not . snd) . reverse

-- | Split a state into chunks of 5
--
-- >>> map show . byChunk . State $ zip [0..] [True, False, True, False, True, False]
-- ["-2: ....#","-1: ...#.","0: ..#.#","1: .#.#.","2: #.#.#","3: .#.#.","4: #.#..","5: .#...","6: #....","7: ....."]
byChunk :: State -> [Chunk]
byChunk (State s) = go (frontPadding ++ s ++ backPadding)
  where
    go (s1:s2:s3:s4:s5:rest) = toChunk [s1, s2, s3, s4, s5] : go (s2:s3:s4:s5:rest)
    go _ = []
    frontPadding = zip [frontIndex..] [False, False, False, False]
      where frontIndex = fst (head s) - 4
    backPadding = zip [backIndex..] [False, False, False, False]
      where backIndex = fst (last s) + 1
    toChunk [(_, a), (_, b), (i, c), (_, d), (_, e)] = Chunk i a b c d e
    toChunk xs = error $ "Bad chunk" ++ show xs

showRaw :: State -> String
showRaw (State s) = show s

-- | Iterate rules givem number of times
--
-- >>> run 20 testRules testInitialState
-- -2: #....##....#####...#######....#.#..##
run :: Int -> [Rule] -> State -> State
run n rules s
  | n > 0 = run (n - 1) rules (apply rules s)
  | n == 0 = s
  | n < 0 = error "bad run"

-- | Sum of number of trues in a State
--
-- >>> sumState . State $ zip [0..] [True, False, True, False, False]
-- 2
-- >>> sumState $ run 20 testRules testInitialState
-- 325
sumState :: State -> Int
sumState (State s) = sum . map fst $ filter snd s

main :: IO ()
main = do
  input <- B.readFile "input/day12.txt"
  let (header : rest) = BC.lines input
  let initialState = readInitial header
  let rules = map readRule . filter ((== '#') . BC.last) $ filter (not . BC.null) rest
  putStrLn $ "day 12 part a: " ++ show (sumState (run 20 rules initialState))
  putStrLn $ "day 12 part b: " ++ "NYI"

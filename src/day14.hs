{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Day14 where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)

newtype Index = Index Int deriving (Enum, Eq, Num, Ord, Show)
newtype Score = Score Int deriving (Eq, Num, Ord, Show)

scoreToIndex :: Score -> Index
scoreToIndex (Score x) = Index x

data State = State
  { a :: Index -- first elf
  , b :: Index -- second elf
  , recipes :: Map Index Score
  }
instance Show State where
  show s = xs ++ " " ++ show ia ++ " " ++ show ib
    where
      n = M.size (recipes s)
      xs = show $ map (\(Score s) -> s) [recipes s M.! Index i | i <- [0 .. n - 1]]
      Index ia = a s
      Index ib = b s

initialState :: State
initialState = State { a = Index 0, b = Index 1, recipes = M.fromList [(Index 0, Score 3), (Index 1, Score 7)] }

-- | Iterate state
--
-- >>> next initialState
-- [3,7,1,0] 0 1
-- >>> next $ next initialState
-- [3,7,1,0,1,0] 4 3
next :: State -> State
next s = s { a = a', b = b', recipes = recipes' }
  where
    newScore = recipes s M.! a s + recipes s M.! b s
    (newScoreTens, newScoreUnits) = splitScore newScore
    newIndex = Index . M.size $ recipes s
    recipes'
      | newScoreTens == 0 = M.insert newIndex newScore (recipes s)
      | otherwise = M.insert newIndex newScoreTens $ M.insert (newIndex + 1) newScoreUnits (recipes s)
    a' = wrapIndex (M.size recipes') $ a s + 1 + scoreToIndex (recipes s M.! a s)
    b' = wrapIndex (M.size recipes') $ b s + 1 + scoreToIndex (recipes s M.! b s)
    splitScore :: Score -> (Score, Score)
    splitScore (Score s) = (Score (s `div` 10), Score (s `mod` 10))
    wrapIndex :: Int -> Index -> Index
    wrapIndex n (Index i) = Index (i `mod` n)

-- | Part a, iterate (n + 10) times and give last 5 score
--
-- >>> map runA [9, 5, 18, 2018]
-- ["5158916779","0124515891","9251071085","5941429882"]
runA :: Int -> String
runA n = map (\(Score s) -> head (show s)) [r M.! Index i | i <- [n .. n + 9]]
    where r = recipes $ iterate next initialState !! (n + 8)

-- | Part b, iterate until last recipes map given sequence, return number of receipes before sequence
--
-- >>> runB [5,1,5,8,9]
-- 9
-- >>> runB [0,1,2,4,5]
-- 5
-- >>> runB [5,9,4,1,4]
-- 2018
-- >>> runB [9,2,5,1,0]
-- 18
runB :: [Int] -> Int
runB target = adjustIndex . lastKey . dropLastRecipeIfNotMatched $ until matchTarget next initialState
  where
    n :: Index = Index $ length target
    target' = map Score target
    -- As we add one or two recipes per cycle, match on the last n and the last n ignoring the final value
    matchTarget :: State -> Bool
    matchTarget s = target' == tail (lastRecipes s) || target' == init (lastRecipes s)
    lastRecipes :: State -> [Score]
    lastRecipes s = [recipes s M.! i  | i <- [lastKey s - n  .. lastKey s], i > 0]
    lastKey :: State -> Index
    lastKey s = fst . fromJust . M.lookupMax $ recipes s
    adjustIndex :: Index -> Int
    adjustIndex (Index i) = i - length target + 1
    dropLastRecipeIfNotMatched:: State -> State
    dropLastRecipeIfNotMatched s
      | tail (lastRecipes s) == target' = s
      | init (lastRecipes s) == target' = s { recipes = M.deleteMax (recipes s) }
      | otherwise = error "Not a match"
      where ls = lastRecipes s

main :: IO ()
main = do
  putStrLn $ "day 14 part a: " ++ runA 157901
  putStrLn $ "day 14 part b: " ++ show (runB [1,5,7,9,0,1])

{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Day14 where

-- import Data.Array (Array)
-- import qualified Data.Array as A
-- import Data.List (foldl', sortBy, tails)
-- import Data.Maybe (catMaybes)
-- import Control.Monad (when)
-- import Control.Monad.Loops (iterateUntilM)
import qualified Data.Map.Strict as M
import Data.Map (Map)

newtype Index = Index Int deriving (Eq, Num, Ord, Show)
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
      xs = show $ map (\(Score s) -> s) [recipes s M.! (Index i) | i <- [0 .. n - 1]]
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
    newScore = (recipes s) M.! (a s) + (recipes s) M.! (b s)
    (newScoreTens, newScoreUnits) = splitScore newScore
    newIndex = Index . M.size $ recipes s
    recipes'
      | newScoreTens == 0 = M.insert newIndex newScore (recipes s)
      | otherwise = M.insert newIndex newScoreTens $ M.insert (newIndex + 1) newScoreUnits (recipes s)
    a' = wrapIndex (M.size recipes') $ (a s) + 1 + scoreToIndex ((recipes s) M.! (a s))
    b' = wrapIndex (M.size recipes') $ (b s) + 1 + scoreToIndex ((recipes s) M.! (b s))

-- | Part a, iterate (n + 10) times and give last 5 score
--
-- >>> map runA [9, 5, 18, 2018]
-- ["5158916779","0124515891","9251071085","5941429882"]
runA :: Int -> String
runA n = map (\(Score s) -> head (show s)) [r M.! (Index i) | i <- [n .. n + 9]]
    where r = recipes $ (iterate next initialState) !! (n + 8)

-- | Divide a two-digit score into its two digits
--
-- >>> splitScore (Score 97)
-- (Score 9,Score 7)
-- >>> splitScore (Score 3)
-- (Score 0,Score 3)
splitScore :: Score -> (Score, Score)
splitScore (Score s) = (Score (s `div` 10), Score (s `mod` 10))

-- | Wrap an index into the range 0..n-1
--
-- >>> map (wrapIndex 5) [Index 3, Index 5, Index 9]
-- [Index 3,Index 0,Index 4]
wrapIndex :: Int -> Index -> Index
wrapIndex n (Index i) = Index (i `mod` n)

--
main :: IO ()
main = do
  putStrLn $ "day 14 part a: " ++ runA 157901
  putStrLn $ "day 14 part b: " ++ "NYI"

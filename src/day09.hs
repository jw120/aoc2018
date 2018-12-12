{-# LANGUAGE ScopedTypeVariables #-}

module Day09 where

import qualified Data.Map as M
import Data.Map (Map)

newtype MarbleIndex = MarbleIndex Int deriving (Eq, Ord)
newtype Player = Player Int deriving (Eq, Ord)

next :: MarbleIndex -> MarbleIndex
next (MarbleIndex x) = MarbleIndex (x + 1)

instance Show MarbleIndex where
  show (MarbleIndex x) = show x

instance Show Player where
  show (Player p) = show p

data Marble = Marble
  { left :: MarbleIndex
  , right :: MarbleIndex
  , value :: Int
  }

hopLeft :: Map MarbleIndex Marble -> MarbleIndex -> MarbleIndex
hopLeft m i = left (m M.! i)

hopRight :: Map MarbleIndex Marble -> MarbleIndex -> MarbleIndex
hopRight m i = right (m M.! i)

data GameState = GameState
  { numPlayers :: Int
  , scores :: Map Player Int -- Players and their scores
  , marbles :: Map MarbleIndex Marble -- Indices and Marbles
  , currentMarbleIndex :: MarbleIndex -- zero-based index into the marbles list
  , nextMarbleValue :: Int
  }

-- For debugging
showMarbleMap :: MarbleIndex -> Map MarbleIndex Marble -> String
showMarbleMap c m = go lowestIndex
  where
    lowestIndex :: MarbleIndex = fst $ M.findMin m
    go :: MarbleIndex -> String
    go i
      | r == lowestIndex = vs
      | otherwise = vs ++ " " ++ go r
      where
        v :: Int = value (m M.! i)
        vs :: String = if i == c then "(" ++ show v ++ ")" else show v
        r :: MarbleIndex = right (m M.! i)

instance Show GameState where
  show g =
    show (numPlayers g) ++ ": " ++
    showMarbleMap (currentMarbleIndex g) (marbles g) ++ " " ++
    show (M.toList (scores g))

-- | Create a new game state
--
-- >>> newGame 3
-- 3: (0) []
newGame :: Int -> GameState
newGame n = GameState
  { numPlayers = n
  , scores = M.empty
  , marbles = M.singleton z (Marble { left = z, right = z, value = 0})
  , currentMarbleIndex = z
  , nextMarbleValue = 1
  }
  where z = MarbleIndex 0

testGame :: [GameState]
testGame = iterate advance $ newGame 9

-- | Advance game state one step
--
-- >>> testGame !! 1
-- 9: 0 (1) []
-- >>> testGame !! 2
-- 9: 0 (2) 1 []
-- >>> testGame !! 3
-- 9: 0 2 1 (3) []
-- >>> testGame !! 4
-- 9: 0 (4) 2 1 3 []
-- >>> testGame !! 9
-- 9: 0 8 4 (9) 2 5 1 6 3 7 []
-- >>> testGame !! 21
-- 9: 0 16 8 17 4 18 9 19 2 20 10 (21) 5 11 1 12 6 13 3 14 7 15 []
-- >>> testGame !! 22
-- 9: 0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15 []
-- >>> testGame !! 23
-- 9: 0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15 [(5,32)]
-- >>> testGame !! 24
-- 9: 0 16 8 17 4 18 19 2 (24) 20 10 21 5 22 11 1 12 6 13 3 14 7 15 [(5,32)]
advance :: GameState -> GameState
advance g
  | nextMarbleValue g `mod` 23 == 0 = specialAdvance g
  | otherwise = g
  { marbles = newMarbles
  , currentMarbleIndex = c
  , nextMarbleValue = 1 + nextMarbleValue g
  }
  where
    -- current marble
    i :: MarbleIndex = currentMarbleIndex g
    -- one marble to the right
    r :: MarbleIndex = hopRight (marbles g) i
    marble_r :: Marble = marbles g M.! r
    -- two marbles to the right
    rr :: MarbleIndex = right marble_r
    marble_rr :: Marble = marbles g M.! rr
    -- new Marble
    c :: MarbleIndex = next . fst $ M.findMax (marbles g)
    newMarbles =
      M.insert r (marble_r { right = c }) .
      M.insert c (Marble { left = r, right = rr, value = nextMarbleValue g}) .
      M.insert rr (marble_rr { left = c }) $
      marbles g

-- Handle divisible by 23 case
specialAdvance :: GameState -> GameState
specialAdvance g = g
   { scores = newScores
   , marbles = newMarbles
   , currentMarbleIndex = l6
   , nextMarbleValue = 1 + nextMarbleValue g
   }
   where
    -- current marble
    i :: MarbleIndex = currentMarbleIndex g
    marble_i :: Marble = marbles g M.! i
    -- six marbles to the left
    h = hopLeft (marbles g)
    l6 :: MarbleIndex = h . h . h . h . h $ h i
    marble_l6 :: Marble = marbles g M.! l6
    -- seven marbles to the left
    l7 :: MarbleIndex = h l6
    marble_l7 :: Marble = marbles g M.! l7
    -- eight marbles to the left
    l8 :: MarbleIndex = h l7
    marble_l8:: Marble = marbles g M.! l8
    newMarbles =
      M.insert l8 (marble_l8 { right = l6 }) .
      M.insert l6 (marble_l6 { left = l8 }) $
      marbles g
    currentPlayer = Player $ (nextMarbleValue g - 1) `mod` numPlayers g + 1
    currentScoreIncrement = nextMarbleValue g + value marble_l7
    newScores = M.insertWith (+) currentPlayer currentScoreIncrement (scores g)

-- | Part a, Highest player score
--
-- >>> highScore 9 25
-- 32
-- >>> highScore 10 1618
-- 8317
-- >>> highScore 13 7999
-- 146373
-- >>> highScore 17 1104
-- 2764
-- >>> highScore 21 6111
-- 54718
-- >>> highScore 30 5807
-- 37305
highScore :: Int -> Int -> Int
highScore players lastMarble = maximum . M.elems $ scores finalState
  where
    finalState = iterate advance (newGame players) !! lastMarble

main :: IO ()
main = do
  putStrLn $ "day 09 part a: " ++ show (highScore 431 70950)
  putStrLn $ "day 09 part b: " ++ show (highScore 431 7095000)

module Day09 where

data GameState = GameState
  { numPlayers :: Int
  , scores :: [(Int, Int)]
  , marbles :: [Int]
  , currentMarbleIndex :: Int -- zero-based index into the marbles list
  , nextMarble :: Int
  }
instance Show GameState where
  show g =
    (show (numPlayers g)) ++ ": " ++
    show (marbles g) ++
    " >" ++ show (currentMarbleIndex g) ++ "< " ++
    show (filter ((>0) . snd) (scores g))

-- | Create a new game state
--
-- >>> newGame 3
-- 3: [0] >0< []
newGame :: Int -> GameState
newGame n = GameState
  { numPlayers = n
  , scores = [(p, 0) | p <- [1..n]]
  , marbles = [0]
  , currentMarbleIndex = 0
  , nextMarble = 1
  }

testGame :: [GameState]
testGame = iterate advance $ newGame 9

-- | Advance game state one step
--
-- >>> testGame !! 1
-- 9: [0,1] >1< []
-- >>> testGame !! 2
-- 9: [0,2,1] >1< []
-- >>> testGame !! 3
-- 9: [0,2,1,3] >3< []
-- >>> testGame !! 4
-- 9: [0,4,2,1,3] >1< []
-- >>> testGame !! 9
-- 9: [0,8,4,9,2,5,1,6,3,7] >3< []
-- >>> testGame !! 22
-- 9: [0,16,8,17,4,18,9,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15] >13< []
-- >>> testGame !! 23
-- 9: [0,16,8,17,4,18,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15] >6< [(5,32)]
-- >>> testGame !! 25
-- 9: [0,16,8,17,4,18,19,2,24,20,25,10,21,5,22,11,1,12,6,13,3,14,7,15] >10< [(5,32)]
advance :: GameState -> GameState
advance g
  | nextMarble g `mod` 23 == 0 = specialAdvance g
  | otherwise = GameState
  { numPlayers = numPlayers g
  , scores = scores g
  , marbles = newMarbles
  , currentMarbleIndex = newCurrentMarbleIndex
  , nextMarble = 1 + nextMarble g
  }
  where
    n = length (marbles g)
    newCurrentMarbleIndex = (currentMarbleIndex g + 1) `mod` n + 1
    (before, after) = splitAt newCurrentMarbleIndex $ marbles g
    newMarbles = before ++ [nextMarble g] ++ after
    newScores = undefined

-- Handle divisible by 23 case
specialAdvance :: GameState -> GameState
specialAdvance g = GameState
  { numPlayers = numPlayers g
  , scores = newScores
  , marbles = newMarbles
  , currentMarbleIndex = removedIndex
  , nextMarble = 1 + nextMarble g
  }
  where
    n = length (marbles g)
    removedIndex = ((currentMarbleIndex g - 7 + n) `mod` n)
    removedValue = marbles g !! removedIndex
    newMarbles = filter (/= removedValue) (marbles g)
    currentPlayer = (nextMarble g - 1) `mod` numPlayers g + 1
    newScores = addScore currentPlayer (nextMarble g + removedValue) (scores g)

-- | Add a score to out list of scores
--
-- >>> addScore 2 7 [(1, 10), (2, 5), (3, 4)]
-- [(1,10),(2,12),(3,4)]
addScore :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
addScore addPlayer addScore = map addIfMatch
  where
    addIfMatch (player, score)
      | player == addPlayer = (player, score + addScore)
      | otherwise = (player, score)

-- | Map int into the range 1..N
--
-- >>> map (circle 5) [1..12]
-- [1,2,3,4,5,1,2,3,4,5,1,2]
circle :: Int -> Int -> Int
circle n x = ((x - 1) `mod` n) + 1

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
highScore players lastMarble = maximum . map snd $ scores finalState
  where
    finalState = (iterate advance (newGame players)) !! lastMarble

main :: IO ()
main = do
  putStrLn $ "day 08 part a: " ++ show (highScore 431 70950)
  putStrLn $ "day 08 part b: " ++ "NYI"

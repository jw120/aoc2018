{-# LANGUAGE ScopedTypeVariables #-}

module Day15 where

import Control.Monad (when)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

--
-- State
--

data State = State
  { cavern :: Array (Int, Int) Bool -- False for walls. Coords (x from left, y from top) (0-indexed)
  , mobs :: Map (Int, Int) Mob
  }

instance Show State where
  show s = init $ unlines [ row y | y <- [0..ymax]]
    where
      ((0, 0), (xmax, ymax)) = A.bounds (cavern s)
      row :: Int -> String
      row j = [toChar (cavern s A.! (x, j)) ((x, j) `M.lookup` (mobs s)) | x <- [0 .. xmax]]
      toChar :: Bool -> Maybe Mob -> Char
      toChar False _ = '#'
      toChar True (Just mob) = if side mob == Elf then 'E' else 'G'
      toChar True Nothing = '.'

newtype Coord = Coord (Int, Int) deriving (Eq, Ord, A.Ix)

-- -- Accessor function gives false for coordinates outside the array
-- isOpen :: State -> (Int, Int) -> Bool
-- isOpen s c
--   | A.inRange (A.bounds (cavern s)) c = cavern s A.! c
--   | otherwise = False

data Side = Elf | Goblin deriving (Eq, Show)

data Mob = Mob
  { side :: Side
  , health :: Int
  } deriving (Show)

initialHealth :: Int = 200

--
-- Mob move logic
--
-- | Move the mob at given coordinates
move :: Bool -> State -> (Int, Int) -> IO State
move log s (x, y) = do
    when log $ print s
    let mySide = case M.lookup (x, y) (mobs s) of
                    Just mob -> side mob
                    Nothing -> error $ "Cannot find myself at " ++ show (x, y)
    when log $ putStrLn ("Moving " ++ show mySide ++ " from " ++ show (x, y))
    let enemyPositions = map fst . M.toList . M.filter ((/= mySide) . side) $ mobs s
    when log $ putStrLn ("Enemies: " ++ show enemyPositions)
    let openPositions = nub . filter (isOpen s) $ concatMap adjacent enemyPositions
    when log $ putStrLn ("Open positions: " ++ show openPositions)
    return s

-- | Return all adjacent coordinates
--
-- >>> sort $ adjacent (2, 1)
-- [(1,1),(2,0),(2,2),(3,1)]
adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- | Is the given coordinate open (i.e. not a mob or wall)
--
isOpen :: State -> (Int, Int) -> Bool
isOpen s c = cavern s A.! c && M.notMember c (mobs s)

-- | Read State from input
--
-- >>> test1
-- #######
-- #.G.E.#
-- #E.G.E#
-- #.G.E.#
-- #######
readState :: [String] -> State
readState xs = State
  { cavern =  A.array ((0, 0), (xMax, yMax)) cavernData
  , mobs = M.fromList mobsData
  }
  where
    (xMax, yMax) = (length (head xs) - 1, length xs - 1)
    tiles :: [((Int, Int), Char)] = zip [(x, y) | y <- [0..yMax], x <- [0..xMax]] (concat xs)
    cavernData :: [((Int, Int), Bool)] = map (\(c, x) -> (c, x `elem` ".GE")) tiles
    mobsData :: [((Int, Int), Mob)] = mapMaybe toMob tiles
    toMob :: ((Int, Int), Char) -> Maybe ((Int, Int), Mob)
    toMob (c, 'E') = Just (c, Mob { side = Elf, health = initialHealth })
    toMob (c, 'G') = Just (c, Mob { side = Goblin, health = initialHealth })
    toMob _ = Nothing

--
-- Test Data
--

test1 :: State
test1 = readState
  [ "#######"
  , "#.G.E.#"
  , "#E.G.E#"
  , "#.G.E.#"
  , "#######"
  ]

test2 :: State
test2 = readState
  [ "#######"
  , "#E..G.#"
  , "#...#.#"
  , "#.G.#G#"
  , "#######"
  ]
--
-- Main
--

main :: IO ()
main = do
  input <- readFile "input/day15.txt"
  let initialState = readState $ lines input
  move True test2 (1,1)
  return ()
--  putStrLn $ "day 15 part a: " ++ "NYI"
--  putStrLn $ "day 15 part b: " ++ "NYI"

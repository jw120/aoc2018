{-# LANGUAGE ScopedTypeVariables #-}

-- TODO - use a Coord data type whose Ord instance does reading order
-- Add attacks to update

module Day15 where

import Control.Monad (foldM, when)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Function (on)
import Data.List (foldl', nub, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

--
-- State
--

data State = State
  { cavern :: Array Coord Bool -- False for walls. Coords (x from left, y from top) (0-indexed)
  , mobs :: Map Coord Mob
  }

instance Show State where
  show s = init $ unlines [ row y | y <- [0..y cMax]]
    where
      (Coord { x = 0, y = 0}, cMax) = A.bounds (cavern s)
      row :: Int -> String
      row j = [toChar (cavern s A.! Coord { x = x, y = j }) ((x, j) `M.lookup` (mobs s)) | x <- [0 .. x cMax]]
      toChar :: Bool -> Maybe Mob -> Char
      toChar False _ = '#'
      toChar True (Just mob) = if side mob == Elf then 'E' else 'G'
      toChar True Nothing = '.'


-- Coord has Ord instance to give "reading order"
data Coord = Coord { x :: Int, y:: Int } deriving (Eq, A.Ix)

instance Ord Coord where
  compare c1 c2 = case compare (y c1) (y c2) of
    LT -> LT
    GT -> GT
    EQ -> compare (x c1) (x c2)

instance Show Coord where
  show c = "(" + show (x c) + "," + show (y c) + ")"

data Side = Elf | Goblin deriving (Eq, Show)

data Mob = Mob
  { side :: Side
  , health :: Int
  } deriving (Show)

initialHealth :: Int = 200

--
-- Mob move logic
--

-- | Update the given mob (with move or attack or nothing)
update :: Bool -> State -> Coord -> IO State
update log s c = do
    when log $ print s
    let mob = fromJust $ M.lookup c (mobs s)
    let neigbours :: [Mob] = mapMaybe (`M.lookup` (mobs s)) $ adjacent c
    let enemeyNeigbours = filter ((/= side mob) . side) neigbours
    if null enemeyNeigbours
      then move log s c
      else return s

-- | Move the mob at given coordinates
move :: Bool -> State -> Coord -> IO State
move log s c = do
    let mob = fromJust $ M.lookup c (mobs s)
    when log $ putStrLn ("Moving " ++ show (side mob) ++ " from " ++ show c)
    let enemyPositions = map fst . M.toList . M.filter ((/= (side mob)) . side) $ mobs s
    when log $ putStrLn ("Enemies: " ++ show enemyPositions)
    let openPositions = nub . filter (isOpen s) $ concatMap adjacent enemyPositions
    when log $ putStrLn ("Open positions: " ++ show openPositions)
    if null openPositions
      then return s
      else do
        let closestPositions = findClosest s (x, y) openPositions
        when log $ putStrLn ("Closest positions: " ++ show closestPositions)
        let destination = firstInReadingOrder closestPositions
        when log $ putStrLn ("Chosen position: " ++ show destination)
        let closestFirstSteps = findClosest s destination . filter (isOpen s) $ adjacent (x, y)
        when log $ putStrLn ("First steps closest to destination: " ++ show closestFirstSteps)
        let firstStep = firstInReadingOrder closestFirstSteps
        when log $ putStrLn ("First step: " ++ show firstStep)
        return $ s { mobs = M.insert firstStep mob (M.delete (x, y) (mobs s)) }

-- | Update every unit
updateAll :: Bool -> State -> IO State
updateAll log s = foldM (update log) s unitPositions
  where unitPositions :: [(Int, Int)] = sort $ M.keys (mobs s)

-- | Return all adjacent coordinates
--
-- >>> sort $ adjacent (2, 1)
-- [(1,1),(2,0),(2,2),(3,1)]
adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- | Return set of point adjacents to the given set
--
-- >>> sort . S.toList . adjacentSet $ S.fromList [(3,3),(3,4)]
-- [(2,3),(2,4),(3,2),(3,5),(4,3),(4,4)]
adjacentSet :: Set (Int, Int) -> Set (Int, Int)
adjacentSet s = S.difference neighbours s
  where neighbours = S.unions . map (S.fromList . adjacent) $ S.toList s

-- | Return the element of the list that is first in reading order
--
-- >>> firstInReadingOrder [(3,5),(2,5),(2,3),(2,4),(3,1),(1,2)]
-- (3,1)
firstInReadingOrder :: [(Int, Int)] -> (Int, Int)
firstInReadingOrder xs@(_:_) = head $ sort xs
firstInReadingOrder [] = error "Empty list in firstInReadingOrder"

-- | return the target(s) that are closest to the start point
findClosest :: State -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findClosest s start targets = go initialVisited initialFrontier
    where
      initialVisited :: Set (Int, Int) = S.empty
      initialFrontier :: Set (Int, Int) = S.singleton start
      go :: Set (Int, Int) -> Set (Int, Int) -> [(Int, Int)]
      go v f
        | S.null i = go v' f'
        | otherwise = S.toList i
        where
          i = (S.intersection f (S.fromList targets))
          v' = S.union v f
          f' = S.difference (S.filter (isOpen s) (adjacentSet f)) v'

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

test3 :: State
test3 = readState
  [ "#########"
  , "#G..G..G#"
  , "#.......#"
  , "#.......#"
  , "#G..E..G#"
  , "#.......#"
  , "#.......#"
  , "#G..G..G#"
  , "#########"
  ]

--
-- Main
--

main :: IO ()
main = do
  input <- readFile "input/day15.txt"
  let initialState = readState $ lines input
  print test3
  s1 <- updateAll False test3
  print s1
  s2 <- updateAll False s1
  print s2
  s3 <- updateAll True s2
  print s3
  return ()
--  putStrLn $ "day 15 part a: " ++ "NYI"
--  putStrLn $ "day 15 part b: " ++ "NYI"

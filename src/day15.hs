{-# LANGUAGE ScopedTypeVariables #-}

module Day15 where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
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
      toChar True (Just (Mob Elf _)) = 'E'
      toChar True (Just (Mob Goblin _)) = 'G'
      toChar True Nothing = '.'

newtype Coord = Coord (Int, Int) deriving (Eq, Ord, A.Ix)

-- Accessor function gives false for coordinates outside the array
isOpen :: State -> (Int, Int) -> Bool
isOpen s c
  | A.inRange (A.bounds (cavern s)) c = cavern s A.! c
  | otherwise = False

data Side = Elf | Goblin

data Mob = Mob Side Int

initialHealth :: Int = 200

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
    toMob (c, 'E') = Just (c, Mob Elf initialHealth)
    toMob (c, 'G') = Just (c, Mob Goblin initialHealth)
    toMob _ = Nothing

-- | Test data
--
-- >>> test1
-- #######
-- #.G.E.#
-- #E.G.E#
-- #.G.E.#
-- #######
test1 :: State
test1 = readState
  [ "#######"
  , "#.G.E.#"
  , "#E.G.E#"
  , "#.G.E.#"
  , "#######"
  ]

main :: IO ()
main = do
  input <- readFile "input/day15.txt"
  let initialState = readState $ lines input
  putStrLn $ "day 12 part a: " ++ "NYI"
  putStrLn $ "day 12 part b: " ++ "NYI"

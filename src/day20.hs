{-# LANGUAGE ScopedTypeVariables #-}

module Day20 where

import           Data.List (foldl')
import           Data.Maybe (isJust, fromJust)
import           Data.Set (Set)
import qualified Data.Set as S

type Position = (Int, Int)
origin :: Position
origin = (0, 0)

data Direction = North | East | South | West deriving (Eq, Ord)
instance Show Direction where
  show North = "N"
  show South = "S"
  show East = "E"
  show West = "W"

step :: Direction -> Position ->Position
North `step` (x, y) = (x, y + 1)
South `step` (x, y) = (x, y - 1)
East `step` (x, y) = (x + 1, y)
West `step` (x, y) = (x - 1, y)

-- | Parse a regex path and return a set of open doors
--
-- We index each grid square by its (left-right, down-up) coordinates with (0,0) as the starting
-- square. Then we hold the partially generated map as a set of doors which are known to be open
--
-- So the example: ENWWW
--
-- #?#?#?#?#
-- ?.|.|.|.?
-- #?#?#?#-#
--     ?X|.?
--     #?#?#
--
-- Is given by the Set containing ( 1,0), ( 2,1), ( 1,2), (-1,2), (-3, 2)
--
-- >>> S.toAscList $ readPath "ENWWW"
-- [(-3,2),(-1,2),(1,0),(1,2),(2,1)]
readPath :: String -> Set Position
readPath s = openDoors
  where
    (_, openDoors, _) = go (S.singleton origin, S.empty, origin) s
    go :: (Set Position, Set Position, Position) -> String -> (Set Position, Set Position, Position)
    go state ('N' : rest) = go (walkStep state North) rest
    go state ('S' : rest) = go (walkStep state South) rest
    go state ('E' : rest) = go (walkStep state East) rest
    go state ('W' : rest) = go (walkStep state West) rest
    go state@(visitedRooms, openDoors, pos) ('(' : rest) = (visitedRooms'', openDoors'', pos')
        where
          (firstBranch : otherBranches, postBranch) = splitBranches rest
          (visitedRooms', openDoors', pos') = go state (firstBranch ++ postBranch)
          (visitedRooms'', openDoors'') = foldl' backtrack (visitedRooms', openDoors') otherBranches
            where
              backtrack :: (Set Position, Set Position) -> String -> (Set Position, Set Position)
              backtrack (rooms, doors) s
                | p' `S.member` rooms = (rooms', doors') -- skip postBranch if we end up in an existing room
                | otherwise = (rooms'', doors'')
                  where
                    (rooms', doors', p') = go (rooms, doors, pos) s
                    (rooms'', doors'', _) = go (rooms', doors', p') postBranch
    go _ (c : rest) = error $ "Unexpected character at '" ++ (c : rest) ++ "'"
    go state "" = state

-- | Split branches from a regex string
--
-- >>> splitBranches "E|SW|)NN"
-- (["E","SW",""],"NN")
-- >>> splitBranches "N(E|S)W|(N|S))(E|S)"
-- (["N(E|S)W","(N|S)"],"(E|S)")
-- >>> splitBranches "NEEE|SSE(EE|N))"
-- (["NEEE","SSE(EE|N)"],"")
splitBranches :: String -> ([String], String)
splitBranches s = fst $ go 0 (([], ""), s)
  where
    go :: Int -> (([String], String), String) -> (([String], String), String)
    go 0 ((acc, current), '|' : rest) = go 0 ((acc ++ [current], ""), rest)
    go 0 ((acc, current), ')' : rest) = ((acc ++ [current], rest), "")
    go n ((acc, current), ')' : rest) = go (n - 1) ((acc, current ++ ")"), rest)
    go n ((acc, current), '(' : rest) = go (n + 1) ((acc, current ++ "("), rest)
    go n ((acc, current), c : rest) = go n ((acc, current ++ [c]), rest)

-- | Walk through door in a given direction from current position updating visited sets
--
walkStep :: (Set Position, Set Position, Position) -> Direction -> (Set Position, Set Position, Position)
walkStep (visitedRooms, openDoors, p) d = (visitedRooms', openDoors', p2)
  where
    p1 = d `step` p
    p2 = d `step` p1
    openDoors' = p1 `S.insert` openDoors
    visitedRooms' = p2 `S.insert` visitedRooms

-- | Flood fill maze starting at origin, returning the number of steps that are needed to reach all rooms
--
-- >>> flood S.empty
-- 0
-- >>> flood $ S.fromList [(0,1)]
-- 1
-- >>> flood $ S.fromList [(0,1), (0,3), (1, 4), (1,0)]
-- 3
flood :: Set Position -> Int
flood openDoors = floodGo (const False) id openDoors 0 S.empty (S.singleton origin)

-- | Flood fill maze starting at origin for given number of steps, returning number rooms reached
--
-- >>> floodTo 0 $ S.fromList [(0,1), (0,3), (1, 4), (1,0)]
-- 0
-- >>> floodTo 1 $ S.fromList [(0,1), (0,3), (1, 4), (1,0)]
-- 1
-- >>> floodTo 3 $ S.fromList [(0,1), (0,3), (1, 4), (1,0)]
-- 4
floodTo :: Int -> Set Position -> Int
floodTo n openDoors = floodGo (>= n) (\_ -> error "Unexpected end") openDoors 0 S.empty (S.singleton origin)

-- Shared flooding helper function, returns number of steps to complete flood fill or number of sites visited with limited number of steps
floodGo :: (Int -> Bool) -> (Int -> Int) -> Set Position -> Int -> Set Position -> Set Position -> Int
floodGo checkStepCount onNull openDoors stepCount visited boundary = case (checkStepCount stepCount, S.null boundary') of
  (True, _) -> S.size visited
  (_, True) -> onNull stepCount
  _ -> floodGo checkStepCount onNull openDoors (stepCount + 1) visited' boundary'
  where
    -- update the set of visited rooms
    visited' = visited `S.union` boundary
    -- update the boundary with adjacent cells that connect via open doors and are unvisited
    adjacents :: Set (Position, Direction)
    adjacents = S.fromList [(p, d) | p <- S.toList boundary, d <- [North, East, South, West]]
    isNew :: (Position, Direction) -> Maybe Position
    isNew (p, d)
      | p1 `S.notMember` openDoors = Nothing
      | p2 `S.member` visited' = Nothing
      | otherwise = Just p2
      where
        p1 = d `step` p
        p2 = d `step` p1
    boundary' = S.map fromJust . S.filter isJust $ S.map isNew adjacents

-- | Number of rooms remaining unvisited after n steps from start
-- assumed rectangular
--
--
-- Example room for "ENWWWSE"
--
--      - - - -   + + +
--      4 3 2 1 0 1 2 3
--   2  . - . - . - .
--   1  |           |
--   0  . - .   * - .
--
-- >>> remaining 0 $ readPath "ENWWWSE"
-- 8
-- >>> remaining 2 $ readPath "ENWWWSE"
-- 6
remaining :: Int -> Set Position -> Int
remaining n doors = totalRooms - floodTo n doors
  where
    totalRooms = xRoomCount * yRoomCount
    xDoorMin = S.findMin $ S.map fst doors
    xDoorMax = S.findMax $ S.map fst doors
    yDoorMin = S.findMin $ S.map snd doors
    yDoorMax = S.findMax $ S.map snd doors
    xRoomCount = 1 + (xDoorMax - xDoorMin) `div` 2
    yRoomCount = 1 + (yDoorMax - yDoorMin) `div` 2

-- | Generate a set of the positions adjacent to the given position
--
-- >>> S.toAscList $ neighbours (3, 4)
-- [(2,4),(3,3),(3,5),(4,4)]
neighbours :: Position -> Set Position
neighbours p = S.fromList [step North p, step East p, step South p, step West p]

-- | Call the solver
--
-- >>> runRegex "^WNE$"
-- 3
-- >>> runRegex "^ENWWW(NEEE|SSE(EE|N))$"
-- 10
-- >>> runRegex "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
-- 18
-- >>> runRegex "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
-- 23
-- >>> runRegex "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
-- 31
runRegex :: String -> Int
runRegex = flood . parseRegex

parseRegex :: String -> Set Position
parseRegex s
  | null s = error "Null string"
  | head s /= '^' = error "Missing ^"
  | last s /= '$' = error "Missing $"
  | otherwise = readPath . init $ tail s

main :: IO ()
main = do
  input <- readFile "input/day20.txt"
  let doors = readPath . tail . init $ init input -- trimming trailing newline and $..^
  putStrLn $ "day 20 part a: " ++ show (flood doors)
  putStrLn $ "day 20 part b: " ++ show (remaining 1000 doors)

{-# LANGUAGE ScopedTypeVariables #-}

module Day18 where

import Data.Array.IArray (Array, (//), (!))
import qualified Data.Array.IArray as A
import Data.List (foldl')
import qualified Data.Map as M
import Data.Map (Map)


--
-- Site holds on site which can be open ground, trees or a lumberyard
---

data Site
  = OpenGround
  | Trees
  | LumberYard
  deriving (Eq)

readSite :: Char -> Site
readSite '.' = OpenGround
readSite '|' = Trees
readSite '#' = LumberYard
readSite c = error $ "Unexpected character (" ++ [c] ++ ")"

instance Show Site where
  show OpenGround = "."
  show Trees = "|"
  show LumberYard = "#"

--
-- Area is the square grid of sites indexed from top-left (0,0) to (xMax, yMax) bottom-right
--

newtype Area = Area (Array (Int, Int) Site) deriving (Eq)

instance Show Area where
  show (Area a) = init $ unlines [ row y | y <- [0 .. yMax]]
    where
      ((0, 0), (xMax, yMax)) = A.bounds a
      row :: Int -> String
      row j = concat [show (a ! (i, j)) | i <- [0 .. xMax]]

-- | Read an Area from a string
--
-- >>> readArea ".#.\n|#|\n||#\n"
-- .#.
-- |#|
-- ||#
readArea :: String -> Area
readArea s
  | all hasValidLength stringRows = Area $ A.array ((0, 0), (size - 1, size - 1)) siteData
  | otherwise = error "Invalid size for Area"
    where
      stringRows :: [String] = lines s
      size = length stringRows
      hasValidLength :: String -> Bool
      hasValidLength = (== size) . length
      toData :: Int -> String -> [((Int, Int), Site)]
      toData y rowStr = zip [(x, y) | x <- [0.. size -1]] $ map readSite rowStr
      siteData :: [((Int, Int), Site)]
      siteData = concat $ zipWith toData [0 .. size - 1] stringRows

-- | Return list with the contents of the sites adjacent to the site whose coordinates are given
--
-- >>> concatMap show $ adjacentSites (0, 0) test0
-- "#.."
-- >>> concatMap show $ adjacentSites (2, 2) test0
-- "...|..|#"
-- >>> concatMap show $ adjacentSites (9, 3) test0
-- "#..#|"
adjacentSites :: (Int, Int) -> Area -> [Site]
adjacentSites (x, y) (Area a) = [ a ! (i, j) | j <- [y - 1 .. y + 1], i <- [x - 1 .. x + 1], valid (i, j)]
  where
    ((0, 0), (xMax, yMax)) = A.bounds a
    valid (c, r) = (c /= x || r /= y) && c >= 0 && c <= xMax && r >=0 && r <= yMax

--
-- Update logic
--

-- | Apply update rules to the area
--
-- >>> update test0 == test1
-- True
-- >>> (iterate update test0) !! 10 == test10
-- True
update :: Area -> Area
update (Area a) = Area $ A.array (A.bounds a) (map updateSite (A.assocs a))
  where
    updateSite :: ((Int, Int), Site) -> ((Int, Int), Site)
    updateSite (coords, s) = (coords, newSite (adjacentSites coords (Area a)) s)
    newSite :: [Site] -> Site -> Site
    newSite surrounds OpenGround
      | length (filter (== Trees) surrounds) >= 3 = Trees
      | otherwise = OpenGround
    newSite surrounds Trees
      | length (filter (== LumberYard) surrounds) >= 3 = LumberYard
      | otherwise = Trees
    newSite surrounds LumberYard
      | LumberYard `notElem` surrounds = OpenGround
      | Trees `notElem` surrounds = OpenGround
      | otherwise = LumberYard

-- | Resource values of an Area
--
-- >>> resources test10
-- 1147
resources :: Area -> Int
resources (Area a) = trees * lumberYards
  where
    trees = length . filter (== Trees) $ A.elems a
    lumberYards = length . filter (== LumberYard) $ A.elems a

test0 :: Area
test0 = readArea $
  ".#.#...|#.\n" ++
  ".....#|##|\n" ++
  ".|..|...#.\n" ++
  "..|#.....#\n" ++
  "#.#|||#|#|\n" ++
  "...#.||...\n" ++
  ".|....|...\n" ++
  "||...#|.#|\n" ++
  "|.||||..|.\n" ++
  "...#.|..|.\n"

test1 :: Area
test1 = readArea . unlines $
  [ ".......##."
  , "......|###"
  , ".|..|...#."
  , "..|#||...#"
  , "..##||.|#|"
  , "...#||||.."
  , "||...|||.."
  , "|||||.||.|"
  , "||||||||||"
  , "....||..|."
  ]

test10 :: Area
test10 = readArea . unlines $
  [ ".||##....."
  , "||###....."
  , "||##......"
  , "|##.....##"
  , "|##.....##"
  , "|##....##|"
  , "||##.####|"
  , "||#####|||"
  , "||||#|||||"
  , "||||||||||"
  ]

main :: IO ()
main = do
  input <- readFile "input/day18.txt"
  let initialArea = readArea input
  let areaList = iterate update initialArea
  let updatedArea = areaList !! 10
  putStrLn $ "day 18 part a: " ++ show (resources updatedArea)
  -- Rather than look for a loop in the areas, instead look at resources
  -- But need to skip initial values to allow sequence to converge
  let skip = 450 -- obtained manually by looking at the generated sequence
  let resourceList = map resources areaList
  let loop = findLoop $ drop skip resourceList
  putStrLn $ "day 18 part b: " ++ show (lookupLoop loop (1000000000 - skip))
--   showSeq 0 initialArea

-- -- Used to explore loop
-- showSeq :: Int -> Area -> IO ()
-- showSeq n a = do
--   putStrLn $ show n ++ ": " ++ show (resources a)
--   showSeq (n + 1) (update a)

-- | Find a repeating pattern in a sequence
-- | Given a sequence x_0, x_1... return the repeating part and the index that the loop starts
--
-- >>> findLoop [0,1,2,3,4,5,3,4,5,3,4,5]
-- ([3,4,5],3)
findLoop :: Ord a => [a] -> ([a], Int)
findLoop xs = (subseq, first)
  where
    subseq = take (firstRep - first) $ drop first xs
    (first, firstRep) = go M.empty 0 xs
    go :: Ord b => Map b Int -> Int -> [b] -> (Int, Int)
    go m i (x: xs) = case M.lookup x m of
      Just n -> (n, i)
      Nothing -> go (M.insert x i m) (i + 1) xs
    go _ _ [] = error "Unexpected empty list in findLoop"

-- | Lookup a value in a repeating sequence
--
-- >>> lookupLoop ([3,4,5],3) 10
-- 4
lookupLoop :: ([x], Int) -> Int -> x
lookupLoop (xs, start) n = xs !! nAroundLoop
  where
    nFromLoopStart = n - start
    nAroundLoop = nFromLoopStart `mod` length xs

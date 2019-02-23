{-# LANGUAGE ScopedTypeVariables #-}

module Day18 where

import Data.Array.IArray (Array, (//), (!))
import qualified Data.Array.IArray as A
import Data.List (foldl')


--
-- Site holds on site which can be open ground, trees or a lumberyard
---

data Site
  = OpenGround
  | Trees
  | LumberYard

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

newtype Area = Area (Array (Int, Int) Site)

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
adjacentSites :: (Int, Int) -> Area -> [Site]
adjacentSites (x, y) (Area a) = [ a ! (i, j) | j <- [y - 1 .. y + 1], i <- [x - 1 .. x + 1], valid (i, j)]
  where
    ((0, 0), (xMax, yMax)) = A.bounds a
    valid (c, r) = (c /= x || r /= y) && c >= 0 && c < xMax && r >=0 && r < yMax

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

main :: IO ()
main = do
  input <- readFile "input/day18.txt"
  let initalArea = readArea input
  putStrLn $ "day 16 part a: " ++ "NYI"
  putStrLn $ "day 16 part b: " ++ "NYI"

{-# LANGUAGE ScopedTypeVariables #-}

module Day22 where

import           Data.Array (Array, (!))
import qualified Data.Array as A
import qualified Data.Ix as Ix

-- | Convert erosion Level to region Type
regionType :: Int -> Int
regionType = (`mod` 3)

-- | Convert geologic index to erosion level
erosionLevel :: Int -> Int -> Int
erosionLevel depth i = (i + depth) `mod` 20183

-- | Geologic indices of regions at depth from (0,0 ) to target (x, y)
--
-- >>> map (geologicIndices 510 (10, 10) !) [(0, 0), (1, 0), (0, 1), (1, 1), (10, 10)]
-- [0,16807,48271,145722555,0]
geologicIndices :: Int -> (Int, Int) -> Array (Int, Int) Int
geologicIndices depth (xMax, yMax) = arr
  where
    arr :: Array (Int, Int) Int
    arr = A.array xyRange [(c, g c) | c <- Ix.range xyRange]
    xyRange = ((0, 0), (xMax, yMax))
    g :: (Int, Int) -> Int
    g (0, 0)  = 0
    g (x, 0) = x * 16807
    g (0, y) = y * 48271
    g (x, y)
      | x == xMax && y == yMax = 0
      | otherwise = erosionLevel depth (arr ! (x - 1, y)) * erosionLevel depth (arr ! (x, y - 1))

-- | Risk level of area with given depth from (0, 0) to target (x, y)
--
-- >>> riskLevel 510 (10, 10)
-- 114
riskLevel :: Int -> (Int, Int) -> Int
riskLevel depth (xMax, yMax) = sum regionTypes
  where
    regionTypes = map (regionType . erosionLevel depth ) . A.elems $ geologicIndices depth (xMax, yMax)

main :: IO ()
main = do
  input <- readFile "input/day22.txt"
  let [line0, line1] = lines input
  let depth = read $ drop (length "depth: ") line0
  let target = read $ "(" ++ drop (length "target: ") line1 ++ ")"
  putStrLn $ "day 22 part a: " ++ show (riskLevel depth target)
  putStrLn $ "day 22 part b: " ++ "NYI"

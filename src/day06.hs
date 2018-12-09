{-# LANGUAGE ScopedTypeVariables #-}

module Day06 where

import qualified Data.Array as A
import Data.Array (Array, Ix)
import Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Function (on)
import Data.List (foldl', maximumBy, minimumBy, nub)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (mapMaybe)

data Point = Point { x :: Int, y :: Int } deriving (Show, Eq, Ord, Ix)

-- | Read a point from a string
--
-- >>> readPoint $ BC.pack "1, 6"
-- Point {x = 1, y = 6}
readPoint :: ByteString -> Point
readPoint = either error id . AC.parseOnly point
  where
    point :: AC.Parser Point
    point = do
      x <- AC.decimal
      AC.char ','
      AC.skipSpace
      y <- AC.decimal
      return $ Point x y

-- Example points from problem statement, ised for doctests
testPoints :: [Point]
testPoints = map (uncurry Point) [(1, 1),(1,6),(8,3),(3,4),(5,5),(8,9)]

-- | Bounding box of a set of point
--
-- >>> boundingBox testPoints
-- (Point {x = 1, y = 1},Point {x = 8, y = 9})
boundingBox :: [Point] -> (Point, Point)
boundingBox ps = (Point (minimum xs) (minimum ys), Point (maximum xs) (maximum ys))
  where
    xs = map x ps
    ys = map y ps

-- | Part a: What is the size of the largest closest-area of the points with finite area
--
-- >>> largestArea testPoints
-- 17
largestArea :: [Point] -> Int
largestArea points = maxArea
  where
    closestArray :: Array Point (Maybe Point) = closest points
    edgePointNearest :: [Point] = nub . mapMaybe (closestArray A.!) $ edgePoints points
    allPointsAndCounts :: [(Point, Int)] = M.toList . counts $ closest points
    interiorPointsAndCounts :: [(Point, Int)] = filter ((`notElem` edgePointNearest) . fst) allPointsAndCounts
    (maxPoint, maxArea) = maximumBy (compare `on` snd) interiorPointsAndCounts

closest:: [Point] -> Array Point (Maybe Point)
closest points = A.array pointRange [(p, closestPoint p points) | p <- A.range pointRange]
  where pointRange = boundingBox points

-- | Count the values in the array
--
-- >>> map (\p -> (counts (closest testPoints)) M.! p) testPoints
-- [7,9,12,9,17,10]
counts :: Array Point (Maybe Point) -> Map Point Int
counts = foldl' f M.empty. A.elems
  where
    f :: Map Point Int -> Maybe Point -> Map Point Int
    f m (Just p) = M.insertWith (+) p 1 m
    f m Nothing = m

-- | Which of the points are on the interior
--
-- >>> interiorPoints testPoints
-- [Point {x = 3, y = 4},Point {x = 5, y = 5}]
interiorPoints :: [Point] -> [Point]
interiorPoints points = filter (\p -> x p /= x p1 && x p /= x p2 && y p /= y p1 && y p /= y p1) points
  where (p1, p2) = boundingBox points

-- | Return all the points on the bounding box edge of the set of points
edgePoints :: [Point] -> [Point]
edgePoints points =
  [Point xMin y | y <- [yMin..yMax]] ++
  [Point x yMax | x <- [xMin..xMax]] ++
  [Point xMax y | y <- [yMin..yMax]] ++
  [Point x yMin | x <- [xMin..xMax]]
    where (Point xMin yMin, Point xMax yMax) = boundingBox points

-- | Manhattan distance between two points
--
-- >>> distance (Point 1 1) (Point 3 0)
-- 3
distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Return the closest point if there is a unique closest
--
-- >>> closestPoint (Point 3 3) testPoints
-- Just (Point {x = 3, y = 4})
-- >>> closestPoint (Point 1 4) testPoints
-- Nothing
closestPoint :: Point -> [Point] -> Maybe Point
closestPoint p points
  | length allClosest == 1 = Just closestPoint
  | otherwise = Nothing
  where
    allClosest = filter (\(_, d) -> d == closestDistance) pointsAndDistances
    (closestPoint, closestDistance) = minimumBy (compare `on` snd) pointsAndDistances
    pointsAndDistances = map (\q -> (q, distance p q)) points

pointsWithTotalDistWithin :: Int -> [Point] -> Int
pointsWithTotalDistWithin maxDist points = length $ filter (\p -> totalDist p <= maxDist) allFeasiblePoints
    where
      totalDist :: Point -> Int
      totalDist p = sum $ map (distance p) points
      (Point xMin yMin, Point xMax yMax) = boundingBox points
      d :: Int = 10000 `div` length points -- a heuristic on how far out we need to go
      allFeasiblePoints :: [Point]
      allFeasiblePoints = [Point x y | x <- [-xMin - d .. xMax + d], y <- [-yMin - d  .. yMax + d]]

main :: IO ()
main = do
  input <- B.readFile "input/day06.txt"
  let points = map readPoint $ BC.lines input
  putStrLn $ "day 06 part a: " ++ show (largestArea points)
  putStrLn $ "day 06 part b: " ++ show (pointsWithTotalDistWithin 10000 points)

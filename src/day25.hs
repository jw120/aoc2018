{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day25 where

import           Data.Either    (fromRight)
import           Data.List      (foldl', partition)
import qualified Data.Set       as S
import           Data.Set       (Set)
import qualified Data.Text      as T
import           Data.Text      (Text)
import qualified Data.Text.IO   as TIO
import qualified Data.Text.Read as TR

type Point = (Int, Int, Int, Int)
type Constellation = Set Point

showPoint :: Point -> String
showPoint (a, b, c, d) = show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d

readPoint :: Text -> Point
readPoint t = (p1, p2, p3, p4)
  where
    [p1, p2, p3, p4] = map readCoord $ T.splitOn "," t
    readCoord :: Text -> Int
    readCoord = fst . fromRight (error "No parse") . TR.signed TR.decimal

nearPoint :: Point -> Point -> Bool
nearPoint (p1, p2, p3, p4) (q1, q2, q3, q4) = d <= 3
  where d = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3) + abs (p4 - q4)

nearConstellation :: Point -> Constellation -> Bool
nearConstellation p = not . S.null . S.filter (nearPoint p)

addPoint :: [Constellation] -> Point -> [Constellation]
addPoint cs p = S.insert p (S.unions near) : far
  where
    (near, far) = partition (nearConstellation p) cs

-- | Allocate points into constellations
--
-- >>> length $ allocatePoints test1
-- 2
-- >>> length $ allocatePoints test2
-- 1
-- >>> length $ allocatePoints test3
-- 3
-- >>> length $ allocatePoints test4
-- 8
allocatePoints :: [Point] -> [Constellation]
allocatePoints = foldl' addPoint []

main :: IO ()
main = do
  input <- TIO.readFile "input/day25.txt"
  let points = map readPoint $ T.lines input
  putStrLn $ "day 24 part a: " ++ show (length (allocatePoints points))
  putStrLn $ "day 24 part b: " ++ "NYI"

test1 :: [Point]
test1 =
  [ (0,0,0,0)
  , (3,0,0,0)
  , (0,3,0,0)
  , (0,0,3,0)
  , (0,0,0,3)
  , (0,0,0,6)
  , (9,0,0,0)
  , (12,0,0,0)
  ]

test2 :: [Point]
test2 = (6, 0, 0, 0) : test1

test3 :: [Point]
test3 =
  [ (1,-1,0,1)
  , (2,0,-1,0)
  , (3,2,-1,0)
  , (0,0,3,1)
  , (0,0,-1,-1)
  , (2,3,-2,0)
  , (-2,2,0,0)
  , (2,-2,0,-1)
  , (1,-1,0,-1)
  , (3,2,0,2)
  ]

test4 :: [Point]
test4 =
  [ (1,-1,-1,-2)
  , (-2,-2,0,1)
  , (0,2,1,3)
  , (-2,3,-2,1)
  , (0,2,3,-2)
  , (-1,-1,1,-2)
  , (0,-2,-1,0)
  , (-2,2,3,-1)
  , (1,2,2,0)
  , (-1,-2,0,-2)
  ]

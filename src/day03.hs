module Day03 where

import Data.Array (Array, listArray, (!), (//))
import Data.Attoparsec.ByteString.Char8 (parseOnly, Parser, char, decimal, skipSpace, endOfLine, maybeResult)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.Foldable (foldl')

fabricSize :: Int
fabricSize = 1000

data Claim = Claim {
  id :: Int,
  x :: Int, -- distance from left edge
  y :: Int, -- distance from top edge
  w :: Int, -- width
  h :: Int  -- height
} deriving (Show)

-- | Parse a Claim from a string
--
-- >>> parseClaim $ BC.pack "#1 @ 1,3: 4x4"
-- Claim {id = 1, x = 1, y = 3, w = 4, h = 4}
parseClaim :: ByteString -> Claim
parseClaim s = case (parseOnly claimParser s) of
  Left msg -> error msg
  Right claim -> claim
  where
    claimParser = do
      char '#'
      id <- decimal
      skipSpace
      char '@'
      skipSpace
      x <- decimal
      char ','
      y <- decimal
      char ':'
      skipSpace
      w <- decimal
      char 'x'
      h <- decimal
      return $ Claim id x y w h

-- Main function for part (a) - number of overlapping squares within the claims
overlaps :: [Claim] -> Int
overlaps claims = foldl' countOverlap 0 fullFabric
  where
    emptyFabric = listArray ((0, 0), (fabricSize - 1, fabricSize - 1)) (repeat 0)
    fullFabric = foldl' addClaim emptyFabric claims
    addClaim :: Array (Int, Int) Int -> Claim -> Array (Int, Int) Int
    addClaim a (Claim _ x y w h) =
      a // [((i, j), a ! (i, j) + 1) | i <- [x .. x + w - 1], j <- [y .. y + h - 1]]
    countOverlap :: Int -> Int -> Int
    countOverlap acc x = acc + if x > 1 then 1 else 0

main :: IO ()
main = do
  input <- B.readFile "input/day03.txt"
  let claims = map parseClaim $ BC.lines input
  putStrLn $ "day 03 part a: " ++ show (overlaps claims)
  putStrLn $ "day 03 part b: NYI"

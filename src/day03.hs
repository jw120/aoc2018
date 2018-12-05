module Day03 where

import Data.Array.Unboxed (UArray, listArray, (!), (//))
import Data.Attoparsec.ByteString.Char8 (parseOnly, Parser, char, decimal, skipSpace, endOfLine, maybeResult)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.Foldable (foldl')


-- Size of the fabric
fabricSize :: Int
fabricSize = 1000

type Fabric = UArray (Int, Int) Int

data Claim = Claim {
  idCode :: Int, -- avoid name 'id' as clashes with preulde
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

-- Main function for part (a) - number of overlapping squares on the fabric
overlaps :: Fabric -> Int
overlaps f = sum [isOverlap (f ! (i, j)) | i <- [0 .. fabricSize - 1], j <- [0 .. fabricSize - 1]]
  where
    isOverlap :: Int -> Int
    isOverlap x = if x > 1 then 1 else 0

emptyFabric :: Fabric
emptyFabric = listArray ((0, 0), (fabricSize - 1, fabricSize - 1)) (repeat 0)

addClaim :: Fabric -> Claim -> Fabric
addClaim a (Claim _ x y w h) =
  a // [((i, j), a ! (i, j) + 1) | i <- [x .. x + w - 1], j <- [y .. y + h - 1]]

-- Main function for part (b) - return the claim that has no overlappes
findUnoverlapped :: Fabric -> [Claim]  -> Int
findUnoverlapped fabric  = idCode . head . filter isUnoverlapped
  where
    isUnoverlapped :: Claim -> Bool
    isUnoverlapped (Claim _ x y w h) = all (== 1) $ patch
      where patch = [fabric ! (i, j) | i <- [x .. x + w - 1], j <- [y .. y + h - 1]]

main :: IO ()
main = do
  input <- B.readFile "input/day03.txt"
  let claims = map parseClaim $ BC.lines input
  let fabric = foldl' addClaim emptyFabric claims
  putStrLn $ "day 03 part a: " ++ show (overlaps fabric)
  putStrLn $ "day 03 part b: " ++ show (findUnoverlapped fabric claims)

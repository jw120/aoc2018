{-# LANGUAGE ScopedTypeVariables #-}

module Day03 where

data Claim = Claim {
  id :: Int,
  x :: Int, -- distance from left edge
  y :: Int, -- distance from top edge
  w :: Int, -- width
  h :: Int  -- height
} deriving (Show)

-- | Parse a Claim from a string
--
-- >>> parseClaim "#1 @ 1,3: 4x4"
-- undefined
parseClaim :: String -> Claim
parseClaim = undefined

main :: IO ()
main = do
  input <- readFile "input/day03.txt"
  let claims = map parseClaim $ lines input
  putStrLn $ "day 03 part a: NYI"
  putStrLn $ "day 03 part b: NYI"

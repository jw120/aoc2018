{-# LANGUAGE ScopedTypeVariables #-}

module Day11 where

-- | Extract the hundreds digit from a positive integer
--
-- >>> map hundredsDigit [1, 21, 342, 4562, 76523]
-- [0,0,3,5,5]
hundredsDigit :: Int -> Int
hundredsDigit x = ((x - x `mod` 100) `div` 100) `mod` 10

-- | Compute power level of a cell
--
-- >>> powerLevel 8 3 5
-- 4
-- >>> powerLevel 57 122 79
-- -5
-- >>> powerLevel 39 217 196
-- 0
-- >>> powerLevel 71 101 153
-- 4
powerLevel :: Int -> Int -> Int -> Int
powerLevel gridSerialNumber x y = hundredsDigit (initialPowerLevel * rackID) - 5
  where
    rackID = x + 10
    initialPowerLevel = rackID * y + gridSerialNumber

main :: IO ()
main = do
  putStrLn "NYI"

module Day05 where

import Data.Char (toLower, isLower, isUpper)
import Data.Function (fix)
import Data.List (nub, sort)

-- | Are the two character reactive (same letter, opposite case)
--
-- >>> zipWith reactive "aAaAa" "AaaAB"
-- [True,True,False,False,False]
reactive :: Char -> Char -> Bool
reactive x y = toLower x == toLower y && isLower x == not (isLower y)

-- | Run one iteration of the reaction on a string
--
-- >>> react1 "dabAcCaCBAcCcaDA"
-- "dabAaCBAcaDA"
-- >>> react1 "dabAaCBAcaDA"
-- "dabCBAcaDA"
-- >>> react1 "dabCBAcaDA"
-- "dabCBAcaDA"
react1 :: String -> String
react1 (x : y : rest)
  | reactive x y = react1 rest
  | otherwise = x : react1 (y : rest)
react1 [x] = [x]
react1 [] = []

-- | Run reeated iteration of the reaction on a string
--
-- >>> react "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
react :: String -> String
react x
  | x == y = x
  | otherwise = react y
  where y = react1 x

-- | Remove one character (upper and lower) and then react
--
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'a'
-- "dbCBcD"
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'B'
-- "daCAcaDA"
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'c'
-- "daDA"
-- >>> reactWithout "dabAcCaCBAcCcaDA" 'D'
-- "abAcCaCBAcCcaA"
reactWithout s x = react $ filter (\c -> toLower c /= toLower x) s

-- | Part b - return shortest string after reacting following removal of one letter
shortest :: String -> Int
shortest s = minimum . map (length . reactWithout s) . nub . sort $ map toLower s

main :: IO ()
main = do
  input <- readFile "input/day05.txt"
  putStrLn $ "day 05 part a: " ++ show (length (react input))
  putStrLn $ "day 05 part b: " ++ show (shortest input)

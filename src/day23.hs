{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day23 where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable (maximumBy)
import           Data.Ord (comparing)

data Bot = Bot {
  x :: Int
, y :: Int
, z :: Int
, r :: Int
}
instance Show Bot where
  show b = "pos=<" ++ show (x b) ++ "," ++ show (y b) ++ "," ++ show (z b) ++ ">, r=" ++ show (r b)

botDistance :: Bot -> Bot -> Int
botDistance b1 b2 = abs (x b1 - x b2) + abs (y b1 - y b2) + abs (z b1 - z b2)

-- | Read a Bot record from a line of input
--
-- >>> readBot (BC.pack "pos=<1,1,2>, r=1")
-- pos=<1,1,2>, r=1
readBot :: ByteString -> Bot
readBot s = either (error . withInput) id . AC.parseOnly bot $ s
  where
    withInput :: String -> String
    withInput msg = concat [msg, " (reading ", BC.unpack s, ")"]
    bot :: AC.Parser Bot
    bot = do
      AC.string "pos=<"
      x <- AC.signed AC.decimal
      AC.char ','
      y <- AC.signed AC.decimal
      AC.char ','
      z <- AC.signed AC.decimal
      AC.string ">, r="
      r <- AC.decimal
      return Bot { x = x, y = y, z = z, r = r}

-- | Solve part A, number of bots in range of the strongest bot
partA :: [Bot] -> Int
partA bots = length $ filter (<= r strongestBot) distances
  where
    strongestBot :: Bot = maximumBy (comparing r) bots
    distances :: [Int] = map (botDistance strongestBot) bots

-- | Test for part A from problem statement
--
-- >>> partA testA
-- 7
testA :: [Bot]
testA = map readBot
  [ "pos=<0,0,0>, r=4"
  , "pos=<1,0,0>, r=1"
  , "pos=<4,0,0>, r=3"
  , "pos=<0,2,0>, r=1"
  , "pos=<0,5,0>, r=3"
  , "pos=<0,0,3>, r=1"
  , "pos=<1,1,1>, r=1"
  , "pos=<1,1,2>, r=1"
  , "pos=<1,3,1>, r=1"
  ]

main :: IO ()
main = do
  input <- B.readFile "input/day23.txt"
  let bots :: [Bot] = map readBot $ BC.lines input
  putStrLn $ "day 23 part a: " ++ show (partA bots)
  putStrLn $ "day 23 part b: " ++ "NYI"

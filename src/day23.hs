{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day23 where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

data Nanobot = Nanobot {
  x :: Int
, y :: Int
, z :: Int
, r :: Int
}
instance Show Nanobot where
  show n = "pos=<" ++ show (x n) ++ "," ++ show (y n) ++ "," ++ show (z n) ++ ">, r=" ++ show (r n)

-- | Read a Nanobot record from a line of input
--
-- >>> readNanobot (BC.pack "pos=<1,1,2>, r=1")
-- pos=<1,1,2>, r=1
readNanobot :: ByteString -> Nanobot
readNanobot = either error id . AC.parseOnly nanobot
  where
    nanobot :: AC.Parser Nanobot
    nanobot = do
      AC.string "pos=<"
      x <- AC.decimal
      AC.char ','
      y <- AC.decimal
      AC.char ','
      z <- AC.decimal
      AC.string ">, r="
      r <- AC.decimal
      return Nanobot { x = x, y = y, z = z, r = r}

main :: IO ()
main = do
  input <- B.readFile "input/day23.txt"
  let bots :: [Nanobot] = map readNanobot $ BC.lines input
  putStrLn $ "day 23 part a: " ++ show (head bots)
  putStrLn $ "day 23 part b: " ++ "NYI"

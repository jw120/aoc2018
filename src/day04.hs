{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

type Guard = Int
type Timestamp = Int -- in range 0..59
type Month = Int -- in range 1-12
type Day = Int -- in range 1-31

-- Actions that are part of our observsations
data ObsAction
  = Wake Timestamp -- Guard wakes up
  | Sleep Timestamp -- Guard sleeps
  | InPeriodGuardChange Guard Timestamp -- new guard
  | EndDayGuardChange Guard -- new guard at end of the day (23:xx)
  deriving (Show)

-- Records of observations
data ObsRecord = ObsRecord Month Day ObsAction deriving (Show)

-- | Parse an observation record
--
-- >>> parseObs $ BC.pack "[1518-11-01 00:00] Guard #10 begins shift"
-- ObsRecord 11 1 (InPeriodGuardChange 10 0)
-- >>> parseObs $ BC.pack "[1518-11-01 00:25] wakes up"
-- ObsRecord 11 1 (Wake 25)
-- >>> parseObs $ BC.pack "[1518-11-01 00:30] falls asleep"
-- ObsRecord 11 1 (Sleep 30)
-- >>> parseObs $ BC.pack "[1518-12-07 23:58] Guard #99 begins shift"
-- ObsRecord 12 7 (EndDayGuardChange 99)
parseObs :: ByteString -> ObsRecord
parseObs s = case (A.parseOnly obsParser s) of
  Left msg -> error msg
  Right obs -> obs
  where
    obsParser :: Parser ObsRecord
    obsParser = do
      A.char '['
      A.decimal -- ignore the year
      A.char '-'
      month <- A.decimal
      A.char '-'
      day <- A.decimal
      A.skipSpace
      hour <- A.decimal
      A.char ':'
      minute <- A.decimal
      A.char ']'
      A.skipSpace
      A.choice
        [ A.string "wakes up" *> pure (ObsRecord month day (Wake minute))
        , A.string "falls asleep" *> pure (ObsRecord month day (Sleep minute))
        , do
            A.string "Guard #"
            guard <- A.decimal
            A.string " begins shift"
            if hour == 0
              then return (ObsRecord month day (InPeriodGuardChange guard minute))
              else return (ObsRecord month day (EndDayGuardChange guard))
        ]

-- Main function for part (a)
sleepiestGuard :: [ObsRecord] -> Int
sleepiestGuard = undefined


main :: IO ()
main = do
  input <- B.readFile "input/day04.txt"
  let observations = map parseObs $ BC.lines input
  putStrLn $ "day 04 part a: " ++ show (sleepiestGuard observations)
  putStrLn $ "day 04 part b: NYI"

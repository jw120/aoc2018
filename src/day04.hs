{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day04 where

import Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Data.List (foldl', nub, sortBy)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromJust) -- horrible hack

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

hour :: ObsAction -> Int
hour (EndDayGuardChange _) = 23
hour _ = 0

minute :: ObsAction -> Int
minute (EndDayGuardChange _) = error "No minute kept for end-day guard change"
minute (Wake t) = t
minute (Sleep t) = t
minute (InPeriodGuardChange _ t) = t

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

cmpObs :: ObsRecord -> ObsRecord -> Ordering
cmpObs (ObsRecord m1 d1 a1) (ObsRecord m2 d2 a2) =
  case (compare m1 m2) of
    GT -> GT
    LT -> LT
    EQ -> case (compare d1 d2) of
      GT -> GT
      LT -> LT
      EQ -> case (compare (hour a1) (hour a2)) of
        GT -> GT
        LT -> LT
        EQ -> compare (minute a1) (minute a2)

data DayAction
  = DayGuard Guard
  | DaySleep Timestamp
  | DayWake Timestamp
  deriving (Show)

-- Replace guard changes before midnight with midnight changes the day of the following record
moveEndDayGuards :: [ObsRecord] -> [ObsRecord]
moveEndDayGuards (r1@(ObsRecord _ _ (EndDayGuardChange g)) : r2@(ObsRecord m2 d2 _) : rest) =
  (ObsRecord m2 d2 (InPeriodGuardChange g 0)) : moveEndDayGuards (r2: rest)
moveEndDayGuards (r1 : r2 : rest) = r1 : moveEndDayGuards (r2: rest)
moveEndDayGuards x = x

groupByDays :: [ObsRecord] -> [[DayAction]]
groupByDays [] = []
groupByDays observations@(first : rest) = (map toDayAction (first : otherFirstDayObs)) :  groupByDays otherDays
  where
    (otherFirstDayObs , otherDays) = span (sameDay first) rest
    sameDay :: ObsRecord -> ObsRecord -> Bool
    sameDay (ObsRecord m1 d1 _) (ObsRecord m2 d2 _) = m1 == m2 && d1 == d2
    toDayAction :: ObsRecord -> DayAction
    toDayAction (ObsRecord _ _ (Wake t)) = DayWake t
    toDayAction (ObsRecord _ _ (Sleep t)) = DaySleep t
    toDayAction (ObsRecord _ _ (InPeriodGuardChange g t)) = DayGuard g
    toDayAction (ObsRecord _ _ (EndDayGuardChange g)) = error "Not expecting end-day"

toSleepIntervals :: [DayAction] -> [(Guard, Timestamp, Timestamp)]
toSleepIntervals actions = xs
    where
      (_, _ , xs) = foldl' f (Nothing, Nothing, []) actions
      f (Just g, Just t1, acc) (DayWake t2) = (Just g, Nothing, acc ++ [(g, t1, t2)])
      f (Just g, Nothing, acc) (DaySleep t) = (Just g, Just t, acc)
      f (_, _, acc) (DayGuard g) = (Just g, Nothing, acc)
      f x y = error $ "Unexpected pattern" ++ show (x, y)

toGuardTotals :: [(Guard, Timestamp, Timestamp)] -> Map Guard Timestamp
toGuardTotals = foldl' f (M.empty)
    where
      f :: Map Guard Timestamp -> (Guard, Timestamp, Timestamp) -> Map Guard Timestamp
      f m (g, t1, t2) = M.insertWith (+) g (t2 - t1) m

 -- Return the key of the map with the highest value (or the first such key if more than one)
keyOfMaxValue :: (Ord k, Ord v) => Map k v -> k
keyOfMaxValue = fst . fromJust . M.foldlWithKey' f Nothing
  where
    f :: Ord v => Maybe (k, v) -> k -> v -> Maybe (k, v)
    f (Just (maxG, maxT)) g t
      | t > maxT = Just (g, t)
      | otherwise = Just (maxG, maxT)
    f Nothing g t = Just (g, t)

maxMinute :: [(Timestamp, Timestamp)] -> Timestamp
maxMinute = keyOfMaxValue . countMinutes
  where
    countMinutes :: [(Timestamp, Timestamp)] -> Map Timestamp Int
    countMinutes = foldl' addInterval zeroCounts
    addInterval :: Map Timestamp Int -> (Timestamp, Timestamp) -> Map Timestamp Int
    addInterval m (t1, t2) = foldl' addMinute m [t | t <- [t1 .. t2 - 1]]
    addMinute :: Map Timestamp Int -> Timestamp -> Map Timestamp Int
    addMinute m t = M.adjust (1 +) t m
    zeroCounts :: Map Timestamp Int
    zeroCounts = M.fromList [(t, 0) | t <- [0 .. 59]]

maxPair :: [(Guard, Timestamp, Timestamp)] -> (Guard, Timestamp)
maxPair triplets = keyOfMaxValue countMinutes
  where
    countMinutes :: Map (Guard, Timestamp) Int
    countMinutes = foldl' addInterval zeroCounts triplets
    addInterval :: Map (Guard, Timestamp) Int -> (Guard, Timestamp, Timestamp) -> Map (Guard, Timestamp) Int
    addInterval m (g, t1, t2) = foldl' (addMinute g) m [t | t <- [t1 .. t2 - 1]]
    addMinute :: Guard -> Map (Guard, Timestamp) Int -> Timestamp -> Map (Guard, Timestamp) Int
    addMinute g m t = M.adjust (1 +) (g, t) m
    zeroCounts :: Map (Guard, Timestamp) Int
    zeroCounts = M.fromList [((g, m), 0) | g <- guards, m <- [0 .. 59]]
    guards :: [Guard]
    guards = nub $ map (\(g, _, _) -> g) triplets

answer :: ByteString -> IO (String, String)
answer input = do
  putStrLn "sbservations"
  putStrLn . unlines $ map show observations
  putStrLn "sortedObservations"
  putStrLn . unlines $ map show sortedObservations
  putStrLn "cleanedObservations"
  putStrLn . unlines $ map show cleanedObservations
  putStrLn "obsByDays"
  putStrLn . unlines $ map show obsByDays
  putStrLn "sleepIntervals"
  putStrLn . unlines $ map show sleepIntervals
  putStrLn "guardTotals"
  putStrLn . unlines $ map show (M.toList guardTotals)
  putStrLn "sleepiestGuard"
  putStrLn $ show sleepiestGuard
  putStrLn "sleepiestGuardIntervals"
  putStrLn . unlines $ map show sleepiestGuardIntervals
  putStrLn "sleepiestMinute"
  putStrLn $ show sleepiestMinute
  return $ (show (sleepiestGuard * sleepiestMinute), show (sleepiestPairGuard * sleepiestPairMinute))
  where
    observations :: [ObsRecord] = map parseObs $ BC.lines input
    sortedObservations :: [ObsRecord] = sortBy cmpObs observations
    cleanedObservations :: [ObsRecord] = moveEndDayGuards  sortedObservations
    obsByDays :: [[DayAction]] = groupByDays cleanedObservations
    sleepIntervals :: [(Guard, Timestamp, Timestamp)] = concatMap toSleepIntervals obsByDays
    guardTotals :: Map Guard Timestamp = toGuardTotals sleepIntervals
    sleepiestGuard :: Guard = keyOfMaxValue guardTotals
    sleepiestGuardIntervals :: [(Timestamp, Timestamp)] =
      map (\(_, t1, t2) -> (t1, t2)) $
      filter (\(g, _, _) -> g == sleepiestGuard) sleepIntervals
    sleepiestMinute :: Timestamp = maxMinute sleepiestGuardIntervals
    (sleepiestPairGuard, sleepiestPairMinute) :: (Guard, Timestamp)= maxPair sleepIntervals

main :: IO ()
main = do
  input <- B.readFile "input/day04.txt"
  (a, b) <- answer input
  putStrLn $ "day 04 part a: " ++ a
  putStrLn $ "day 04 part b: " ++ b


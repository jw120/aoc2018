{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day24 where

import           Control.Applicative ((<|>), (<*))
import           Control.Monad (when)
import           Data.Attoparsec.ByteString.Char8 (Parser, char, choice, decimal, many', sepBy1,
                                                    parseOnly, parseTest, skipWhile, string, skipSpace)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Foldable (maximumBy)
import           Data.Functor (($>))
import           Data.List (foldl', sortBy)
import qualified Data.Map as M
import           Data.Map (Map, (!))
import           Data.Ord (comparing)

newtype GroupIndex = GroupIndex Int deriving (Eq, Ord)
instance Show GroupIndex
  where
    show (GroupIndex i) = show i
type State = Map GroupIndex Group
printState :: State -> IO ()
printState s = putStrLn $ "Immune System:\n" ++ showGroups immuneSystem ++ "Infection:\n" ++ showGroups infection
  where
    immuneSystem :: State = M.filter ((== ImmuneSystem) . side) s
    infection :: State = M.filter ((== Infection) . side) s
    showGroups :: State -> String
    showGroups = unlines . map (\(GroupIndex i, g) -> pad i ++ show i ++ ": " ++ show g) . M.assocs
    pad x = if x < 10 then " " else ""

data Group = Group {
  n :: Int
, hp :: Int
, atk :: Int
, atkType :: AtkType
, weak :: [AtkType]
, immune :: [AtkType]
, initiative :: Int
, side :: Side
}
instance Show Group where
  show g = "  " ++ show (n g) ++ " x " ++ show (hp g) ++
    " (" ++ show (atk g) ++ " " ++ show (atkType g) ++ ", " ++
    show [weak g, immune g] ++ ", " ++ show (side g) ++ ")"

data AtkType = Cold | Fire | Bludgeoning | Slashing | Radiation deriving (Eq, Show)
data Side = ImmuneSystem | Infection deriving (Eq, Show)

-- | Read initial state
readState :: ByteString -> State
readState = either error id . parseOnly state
  where
    state :: Parser State
    state = do
      immuneSystem <- "Immune System:" *> skipSpace *> many' (group ImmuneSystem <* skipSpace)
      infection <- skipSpace *> "Infection:" *> skipSpace *> many' (group Infection <* skipSpace)
      return . M.fromList $ zip (map GroupIndex [0..]) (immuneSystem ++ infection)

-- | Parse group
group :: Side -> Parser Group
group side = do
  n <- decimal
  hp <- " units each with " *> decimal
  (weak, immune) <-  " hit points " *> weaknesses
  atk <- skipSpace *> "with an attack that does " *> decimal
  t <- skipSpace *> attackType
  initiative <- " damage at initiative " *> decimal
  return Group {
    n = n, hp = hp,
    atk = atk, atkType = t, initiative = initiative,
    weak = weak, immune = immune, side = side
  }

-- | Parse possible list of weaknesses and immgroupies
--
-- >>> parseTest weaknesses $ pack "with an attack"
-- Done "with an attack" ([],[])
-- >>> parseTest weaknesses $ pack "(weak to bludgeoning; immune to slashing, fire)"
-- Done "" ([Bludgeoning],[Slashing,Fire])
-- >>> parseTest weaknesses $ pack "(weak to bludgeoning; immune to cold)"
-- Done "" ([Bludgeoning],[Cold])
weaknesses :: Parser ([AtkType], [AtkType])
weaknesses = weaknessList <|> return ([], [])
  where
    weaknessList :: Parser ([AtkType], [AtkType])
    weaknessList = combineClauses <$> (char '(' *> many' weaknessClause <* char ')')
    weaknessClause :: Parser (Bool, [AtkType])
    weaknessClause = do
      isImmune <- ("immune to " $> True) <|> ("weak to " $> False)
      types <- sepBy1 attackType ", "
      skipWhile (== ';')
      skipSpace
      return (isImmune, types)
    combineClauses :: [(Bool, [AtkType])] -> ([AtkType], [AtkType])
    combineClauses [(True, xs)] = ([], xs)
    combineClauses [(False, xs)] = (xs, [])
    combineClauses [(True, xs), (False, ys)] = (ys, xs)
    combineClauses [(False, xs), (True, ys)] = (xs, ys)
    combineClauses x = error $ "Bad clauses " ++ show x

-- | Parse an attack type
--
-- >>> parseTest attackType $ pack "fire"
-- Done "" Fire
attackType :: Parser AtkType
attackType = choice
  [ "cold" $> Cold
  , "fire" $> Fire
  , "slashing" $> Slashing
  , "bludgeoning" $> Bludgeoning
  , "radiation" $> Radiation
  ]

-- | Run one battle round with optional commentary
runOne :: Bool -> State -> IO State
runOne log s = do
  when log $ putStrLn "Running"
  let allGroupsByPower :: [GroupIndex] = indicesByEffPower s
  when log $ putStrLn ("Selection order: " ++ show allGroupsByPower)
  let targetAssignments :: [(GroupIndex, GroupIndex)] = foldl' (assignTarget s) [] allGroupsByPower
  when log $ putStrLn ("Targets: " ++ show targetAssignments)
  -- in order of atkpower, each group chooses zero/one targets (only one group can target each group)
  -- in order of initiative, attack
  return s

-- Select a target for the group and return the index of the selected group
assignTarget :: State -> [(GroupIndex, GroupIndex)] -> GroupIndex -> [(GroupIndex, GroupIndex)]
assignTarget s assigned i = undefined

-- | Return the effective power of a group
-- >>> map (\g -> (n g, effPower g)) $ M.elems exampleState
-- [(17,76619),(989,24725),(801,92916),(4485,53820)]
effPower :: Group -> Int
effPower u = n u * atk u

-- | Return the group indicies sorted in descending order of attack power (with tiebreakers)
--
-- >>> map (\i -> n (exampleState ! i)) $ indicesByEffPower exampleState
-- [801,17,4485,989]
indicesByEffPower :: State -> [GroupIndex]
indicesByEffPower s = sortBy (flip comparePower) $ M.keys s
  where
    comparePower :: GroupIndex -> GroupIndex -> Ordering
    comparePower i1 i2 = case compare (effPower u1) (effPower u2) of
       EQ -> compare (initiative u1) (initiative u2)
       x -> x
      where
        u1 = s ! i1
        u2 = s ! i2

main :: IO ()
main = do
  input <- B.readFile "input/day24.txt"
  let state = readState input
  printState state
  putStrLn ""
  runOne True state
  putStrLn $ "day 24 part a: " ++ "NYI"
  putStrLn $ "day 24 part b: " ++ "NYI"

exampleState :: State
exampleState = readState "\
  \Immune System:\n\
  \17 units each with 5390 hit points (weak to radiation, bludgeoning) \
  \with an attack that does 4507 fire damage at initiative 2\n\
  \989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) \
  \with an attack that does 25 slashing damage at initiative 3\n\n\
  \Infection:\n\
  \801 units each with 4706 hit points (weak to radiation) \
  \with an attack that does 116 bludgeoning damage at initiative 1\n\
  \4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) \
  \with an attack that does 12 slashing damage at initiative 4\n"
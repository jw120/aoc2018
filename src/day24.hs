{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day24 where

import           Control.Applicative ((<|>), (<*))
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable (maximumBy)
import           Data.Functor (($>))
import           Data.Ord (comparing)

data State = State
  { immuneSystem :: [Unit]
  , infection :: [Unit]
  }
instance Show State where
  show s = "Immune System:\n" ++ units (immuneSystem s) ++ "Infection:\n" ++ units (infection s)
    where
      units :: [Unit] -> String
      units = unlines . map show

data Unit = Unit {
  n :: Int
, hp :: Int
, atk :: Int
, atkType :: AtkType
, weak :: [AtkType]
, immune :: [AtkType]
, initiative :: Int
}
instance Show Unit where
  show u = "  " ++ show (n u) ++ " x " ++ show (hp u) ++
    " (" ++ show (atk u) ++ " " ++ show (atkType u) ++ ", " ++ show [weak u, immune u] ++ ")"

data AtkType = Cold | Fire | Bludgeoning | Slashing | Radiation deriving (Eq, Show)

-- | Read initial state
readState :: ByteString -> State
readState = either error id . AC.parseOnly state
  where
    state :: AC.Parser State
    state = do
      AC.string "Immune System:"
      AC.skipSpace
      immuneSystem <- AC.many' (unit <* AC.skipSpace)
      AC.skipSpace
      AC.string "Infection:"
      AC.skipSpace
      infection <- AC.many' (unit <* AC.skipSpace)
      return State { immuneSystem = immuneSystem, infection = infection }

-- | Parse unit
unit :: AC.Parser Unit
unit = do
  n <- AC.decimal
  AC.string " units each with "
  hp <- AC.decimal
  AC.string " hit points "
  (weak, immune) <- weaknesses
  AC.skipSpace
  AC.string "with an attack that does "
  atk <- AC.decimal
  AC.skipSpace
  t <- attackType
  AC.string " damage at initiative "
  initiative <- AC.decimal
  return Unit {
    n = n, hp = hp,
    atk = atk, atkType = t, initiative = initiative,
    weak = weak, immune = immune
  }

-- | Parse possible list of weaknesses and immunities
--
-- >>> AC.parseOnly weaknesses $ BC.pack "with an attack"
-- Right ([],[])
-- >>> AC.parseOnly weaknesses $ BC.pack "(weak to bludgeoning; immune to slashing, fire)"
-- Right ([Bludgeoning],[Slashing,Fire])
-- >>> AC.parseOnly weaknesses $ BC.pack "(weak to bludgeoning; immune to cold)"
-- Right ([Bludgeoning],[Cold])
weaknesses :: AC.Parser ([AtkType], [AtkType])
weaknesses = weaknessList <|> return ([], [])
  where
    weaknessList :: AC.Parser ([AtkType], [AtkType])
    weaknessList = do
      AC.char '('
      clauses <- AC.many' weaknessClause
      AC.char ')'
      return $ combineClauses clauses
    weaknessClause :: AC.Parser (Bool, [AtkType])
    weaknessClause = do
      isImmune <- (AC.string "immune to " $> True) <|> (AC.string "weak to " $> False)
      types <- AC.sepBy1 attackType (AC.string ", ")
      AC.skipWhile (== ';')
      AC.skipSpace
      return (isImmune, types)
    combineClauses :: [(Bool, [AtkType])] -> ([AtkType], [AtkType])
    combineClauses [(True, xs)] = ([], xs)
    combineClauses [(False, xs)] = (xs, [])
    combineClauses [(True, xs), (False, ys)] = (ys, xs)
    combineClauses [(False, xs), (True, ys)] = (xs, ys)
    combineClauses x = error $ "Bad clauses " ++ show x

-- | Parse an attack type
--
-- >>> AC.parseOnly attackType $ BC.pack "fire"
-- Right Fire
attackType :: AC.Parser AtkType
attackType = AC.choice
  [ AC.string "cold" $> Cold
  , AC.string "fire" $> Fire
  , AC.string "slashing" $> Slashing
  , AC.string "bludgeoning" $> Bludgeoning
  , AC.string "radiation" $> Radiation
  ]

main :: IO ()
main = do
  input <- B.readFile "input/day24.txt"
  let state = readState input
  print state
  putStrLn $ "day 24 part a: " ++ "NYI"
  putStrLn $ "day 24 part b: " ++ "NYI"


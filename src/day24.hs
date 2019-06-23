{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day24 where

import           Control.Applicative ((<|>), (<*))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, choice, decimal, many', sepBy1,
                                                    parseOnly, parseTest, skipWhile, string, skipSpace)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
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
readState = either error id . parseOnly state
  where
    state :: Parser State
    state = do
      immuneSystem <- "Immune System:" *> skipSpace *> many' (unit <* skipSpace)
      infection <- skipSpace *> "Infection:" *> skipSpace *> many' (unit <* skipSpace)
      return State { immuneSystem = immuneSystem, infection = infection }

-- | Parse unit
unit :: Parser Unit
unit = do
  n <- decimal
  hp <- " units each with " *> decimal
  (weak, immune) <-  " hit points " *> weaknesses
  atk <- skipSpace *> "with an attack that does " *> decimal
  t <- skipSpace *> attackType
  initiative <- " damage at initiative " *> decimal
  return Unit {
    n = n, hp = hp,
    atk = atk, atkType = t, initiative = initiative,
    weak = weak, immune = immune
  }

-- | Parse possible list of weaknesses and immunities
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

main :: IO ()
main = do
  input <- B.readFile "input/day24.txt"
  let state = readState input
  print state
  putStrLn $ "day 24 part a: " ++ "NYI"
  putStrLn $ "day 24 part b: " ++ "NYI"

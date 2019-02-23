{-# LANGUAGE ScopedTypeVariables #-}

module Day19 where

import Data.Array.IArray (Array, (//), (!))
import qualified Data.Array.IArray as A
import Data.Bits
import Data.Char (toUpper, toLower)
import Data.List (foldl')

type Registers = Array Int Int -- alwats size 6

data State = State
  { ipBinding :: Int
  , ipValue :: Int
  , registers :: Registers
  }

instance Show State where
  show s = show (ipValue s) ++ ":" ++ show (A.elems (registers s))

initialState :: Int -> State
initialState ipAssignment = State
  { ipBinding = ipAssignment
  , ipValue = 0
  , registers = A.listArray (0, 5) [0, 0, 0, 0, 0 ,0]
  }

data OpCode =
   Addr | Addi | Mulr | Muli
  | Seti | Setr
  | Banr | Bani | Borr | Bori
  | Gtir | Gtri | Gtrr
  | Eqir | Eqri | Eqrr
  deriving (Enum, Eq, Read, Show)

data Instruction = Instruction
  { opCode :: OpCode
  , insA :: Int
  , insB :: Int
  , insC :: Int
  }

instance Show Instruction where
  show i = unwords
    [ show (opCode i)
    , show (insA i)
    , show (insB i)
    , show (insC i)
    ]

type Program = Array Int Instruction

-- | Read an instruction
--
-- >>> readInstruction "seti 6 0 2"
-- Seti 6 0 2
readInstruction :: String -> Instruction
readInstruction s = Instruction
  { opCode = read $ capitalizeFirst opStr
  , insA = read aStr
  , insB = read bStr
  , insC = read cStr
  }
  where
    [opStr, aStr, bStr, cStr] = words s
    capitalizeFirst (first : rest) = toUpper first : map toLower rest

readProgram :: [String] -> Program
readProgram ss = A.listArray (0, length ixs - 1) ixs
  where
    ixs = map readInstruction ss

-- | Read the IP assignment line
--
-- >>> readIP "#ip 4"
-- 4
readIP :: String -> Int
readIP s = read ipStr
  where
    ["#ip", ipStr] = words s

-- | Step one instruction of the program
--
-- >>> step (initialState 0) testProgram
-- 1:[0,5,0,0,0,0]
-- >>> step (step (initialState 0) testProgram) testProgram
-- 2:[1,5,6,0,0,0]
-- >>> step (step (step (initialState 0) testProgram) testProgram) testProgram
-- 4:[3,5,6,0,0,0]
-- >>> step (step (step (step (initialState 0) testProgram) testProgram) testProgram) testProgram
-- 6:[5,5,6,0,0,0]
-- >>> step (step (step (step (step (initialState 0) testProgram) testProgram) testProgram) testProgram) testProgram
-- 7:[6,5,6,0,0,9]
step :: State -> Program -> State
step s p = s { registers = r', ipValue = ipValue' }
  where
    instruction = p ! ipValue s
    r = registers s // [(ipBinding s, ipValue s)]
    r' = r // [(c, newVal)]
    ipValue' = (r' ! ipBinding s) + 1
    (a, b, c) = (insA instruction, insB instruction, insC instruction)
    newVal = case opCode instruction of
      Addr -> r ! a + r ! b
      Addi -> r ! a + b
      Mulr -> r ! a * r ! b
      Muli -> r ! a * b
      Banr -> r ! a .&. r ! b
      Bani -> r ! a .&. b
      Borr -> r ! a .|. r ! b
      Bori -> r ! a .|. b
      Gtir -> if a > r ! b then 1 else 0
      Gtri -> if r ! a > b then 1 else 0
      Gtrr -> if r ! a > r ! b then 1 else 0
      Eqir -> if a == r ! b then 1 else 0
      Eqri -> if r ! a == b then 1 else 0
      Eqrr -> if r ! a == r ! b then 1 else 0
      Setr -> r ! a
      Seti -> a

-- | Run the program until it halts
--
-- >>> run 0 testProgram
-- 7:[6,5,6,0,0,9]
run :: Int -> Program -> State
run ipAssignment p = go (initialState ipAssignment)
  where
    go :: State -> State
    go s
      | ipValue s >= 0 && ipValue s < length p = go (step s p)
      | otherwise = s

testProgram :: Program
testProgram = readProgram
  [ "seti 5 0 1"
  , "seti 6 0 2"
  , "addi 0 1 0"
  , "addr 1 2 3"
  , "setr 1 0 0"
  , "seti 8 0 4"
  , "seti 9 0 5"
  ]

main :: IO ()
main = do
  input <- readFile "input/day19.txt"
  let (ipStr : progStr) = lines input
  let finalState = run (readIP ipStr) (readProgram progStr)
  let reg0 = registers finalState ! 0
  putStrLn $ "day 19 part a: " ++ show reg0
  putStrLn $ "day 19 part b: " ++ "NYI"

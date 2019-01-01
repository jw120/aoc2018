{-# LANGUAGE ScopedTypeVariables #-}

module Day16 where

import Data.Array.IArray (Array, (//), (!))
import qualified Data.Array.IArray as A
import Data.Bits
import Data.List (foldl', stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Instruction = Array Int Int
type Registers = Array Int Int

data Operation
  = Addr | Addi | Mulr | Muli
  | Seti | Setr
  | Banr | Bani | Borr | Bori
  | Gtir | Gtri | Gtrr
  | Eqir | Eqri | Eqrr
  deriving (Enum, Eq, Show)

allOperations :: [Operation]
allOperations = [Addr .. Eqrr]

-- Result of applying the given opcode with the interpreted as the given operation
applyAs ::  Operation -> Instruction -> Registers -> Registers
applyAs op i r = r // [(c, newVal)]
  where
    (a, b, c) = (i ! 1, i ! 2, i ! 3)
    newVal = case op of
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

-- | Which Operations are possible interpretations of the instruction and before/after registers
--
-- >>> possOps ((ar [3,2,1,1]),(ar [9,2,1,2]),(ar [3,2,2,1]))
-- [Addi,Mulr,Seti]
possOps :: (Registers, Instruction, Registers) -> [Operation]
possOps (r, i, r') = filter poss allOperations
  where
    poss :: Operation -> Bool
    poss op = applyAs op i r == r'

-- Assign possible operations to each opcode
assignOpcodes :: [(Registers, Instruction, Registers)] -> Map Int [Operation]
assignOpcodes = foldl' assign M.empty
    where
      assign :: Map Int [Operation] -> (Registers, Instruction, Registers) -> Map Int [Operation]
      assign m (r, i, r') = case M.lookup code m of
        Just p -> M.insert code (filter (`elem` p') p) m
        Nothing -> M.insert code p' m
        where
          code = i ! 0
          p' = possOps (r, i, r')

-- Allocate opcodes to each instruction
deduceOpcodes :: Map Int [Operation] -> Map Int Operation
deduceOpcodes m = snd $ go (m, M.empty)
  where
    go :: (Map Int [Operation], Map Int Operation) -> (Map Int [Operation], Map Int Operation)
    go (m, assigned)
      | M.size assigned == M.size m = (m, assigned)
      | otherwise = go (m', assigned')
      where
        -- First code that is unambiguos
        (code, [instruction]) :: (Int, [Operation]) = M.findMin $ M.filter ((== 1) . length) m
        m' = M.map (filter (/= instruction)) m
        assigned' = M.insert code instruction assigned

-- run the program
run :: Map Int Operation -> [Instruction] -> Registers
run m = foldl' f zeros
  where
    zeros = A.listArray (0, 3) [0, 0, 0, 0]
    f :: Registers -> Instruction -> Registers
    f r i = applyAs (m M.! (i ! 0)) i r

-- | Read a sample from 4 strings
--
-- >>> readSample ("Before: [1, 0, 2, 1]", "2 3 2 0", "After:  [1, 0, 2, 4]", "")
-- (array (0,3) [(0,1),(1,0),(2,2),(3,1)],array (0,3) [(0,2),(1,3),(2,2),(3,0)],array (0,3) [(0,1),(1,0),(2,2),(3,4)])
readSample :: (String, String, String, String) -> (Registers, Instruction, Registers)
readSample (a, b, c, _) = (A.listArray (0, 3) a', A.listArray (0,3) b', A.listArray (0, 3) c')
    where
      a' :: [Int] = read . fromJust $ stripPrefix "Before: " a
      b' :: [Int] = map read $ words b
      c' :: [Int] = read . fromJust $ stripPrefix "After:  " c

-- | Read an instruction
--
-- >> readInstruction "10 0 1 3"
-- array (0,3) [(0,10),(1,0),(2,1),(3,1)]
readInstruction :: String -> Instruction
readInstruction = A.listArray (0, 3) . map read . words

-- Helper function to convert a length-4 list to an array
ar :: [Int] -> Array Int Int
ar xs
    | length xs == 4 = A.listArray (0, 3) xs
    | otherwise = error "Bad list length in ar"

-- Bunch a list into a list of 4-tuples
bunch :: [x] -> [(x, x, x, x)]
bunch [] = []
bunch (a:b:c:d:rest) = (a,b,c,d) : bunch rest
bunch _ = error "List length not a multiple of 4 in bunch"

main :: IO ()
main = do
  inputA <- readFile "input/day16a.txt"
  let samples = map readSample . bunch $ lines inputA
  putStrLn $ "day 16 part a: " ++ show (length (filter ((>= 3) . length) (map possOps samples)))
  let opcodeAssignments= assignOpcodes samples
  let opcodeDeductions = deduceOpcodes opcodeAssignments
  inputB <- readFile "input/day16b.txt"
  let program = map readInstruction $ lines inputB
  let output = run opcodeDeductions program
  putStrLn $ "day 16 part b: " ++ show (output ! 0)

module BDD (
  BDDFun,
  pick,
  bddAsc,
  bddDesc,
  allBDDFuns
) where
import           Data.DecisionDiagram.BDD (AscOrder, BDD, DescOrder, ItemOrder,
                                           false, ite, true, var)

type BDDFun o = BDD o

pick :: ItemOrder o => Int -> BDDFun o -> BDDFun o -> BDDFun o
pick i a0 a1 = ite (var i) a1 a0

bddAsc :: BDDFun AscOrder -> BDDFun AscOrder
bddAsc = id

bddDesc :: BDDFun DescOrder -> BDDFun DescOrder
bddDesc = id

-- Generating all BDDFuns

allBDDFuns :: ItemOrder o => Int -> [BDDFun o]
allBDDFuns n = map (bddFromOutput n) $ outputPermutations n

boolToBDD :: Bool -> BDDFun o
boolToBDD True  = true
boolToBDD False = false

bddFromOutput :: ItemOrder o => Int -> [Bool] -> BDDFun o
bddFromOutput bits = bddFromOutput' bits 0

bddFromOutput' :: ItemOrder o => Int -> Int -> [Bool] -> BDDFun o
bddFromOutput' 0 varN out = boolToBDD (out !! varN)
bddFromOutput' bits varN out = pick bits
  (bddFromOutput' (bits - 1) (2 * varN + 1) out)
  (bddFromOutput' (bits - 1) (2 * varN) out)

outputPermutations :: Int -> [[Bool]]
outputPermutations n = permutations (2^n)

permutations :: Int -> [[Bool]]
permutations 0 = [[]]
permutations n = do
  v <- [False, True]
  vs <- permutations (n - 1)
  return (v : vs)

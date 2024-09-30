module BDD (
  BDDFun,
  pick,
  bddAsc,
  bddDesc,
  allBDDFuns,
  bddFromOutput,
  normalizeBDD
) where
import           Data.DecisionDiagram.BDD (AscOrder, BDD, DescOrder, ItemOrder,
                                           Sig (SBranch, SLeaf), false,
                                           fromGraph, ite, support, toGraph,
                                           true, var)
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IM
import qualified Data.IntSet              as IS

type BDDFun = BDD AscOrder

pick :: ItemOrder o => Int -> BDD o -> BDD o -> BDD o
pick i a0 a1 = ite (var i) a1 a0

bddAsc :: BDD AscOrder -> BDD AscOrder
bddAsc = id

bddDesc :: BDD DescOrder -> BDD DescOrder
bddDesc = id

-- Generating all BDDFuns

allBDDFuns :: ItemOrder o => Int -> [BDD o]
allBDDFuns n = map (bddFromOutput n) $ outputPermutations n

boolToBDD :: Bool -> BDD o
boolToBDD True  = true
boolToBDD False = false

bddFromOutput :: ItemOrder o => Int -> [Bool] -> BDD o
bddFromOutput bits = bddFromOutput' bits 0

bddFromOutput' :: ItemOrder o => Int -> Int -> [Bool] -> BDD o
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

-------------------- Normalization -------------------------------

-- The problem that comparisons of BDDs take variable indeces into account.
-- The fact is that two BDDs for the same function and item order are
-- unique up to isomorphism. Thus, the same function CAN have different BDDs
-- describing it.
-- normalizeBDD ensures that for n variables and an ascending order,
-- the function will return an equivalent function with variables [0 .. n - 1].

-- The logic here is that if we assume an ascending order, then we should be able to simply
-- map the variable indices to [0 .. n - 1] where n is the number of variables, without changing
-- the order.
-- We assume that the second parameter in the result of toGraph represents the number of
-- nodes in the graph, which shouldn't change by simply changing the indeces.

-- TODO-NEW: We could check if the BDD is already normalized first.
normalizeBDD :: BDD AscOrder -> BDD AscOrder
normalizeBDD bdd = fromGraph (g', n)
  where
    vars = support bdd
    orderMapping = IM.fromAscList $ zip (IS.toAscList vars) [0 ..]
    (g, n) = toGraph bdd
    g' = IM.map (mapOrder orderMapping) g

mapOrder :: IntMap Int -> Sig Int -> Sig Int
mapOrder _ s@(SLeaf _)              = s
mapOrder orderMap (SBranch i lo hi) = SBranch (orderMap IM.! i) lo hi

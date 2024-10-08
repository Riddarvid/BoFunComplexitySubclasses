{-# LANGUAGE FlexibleInstances #-}
module BDD (
  BDDFun,
  pick,
  bddAsc,
  bddDesc,
  allBDDFuns,
  bddFromOutput,
  normalizeBDD,
  flipInputs,
  isConstBDD
) where
import           Data.DecisionDiagram.BDD (AscOrder, BDD (Leaf), DescOrder,
                                           ItemOrder, Sig (SBranch, SLeaf),
                                           false, fromGraph, ite, notB,
                                           substSet, support, toGraph, true,
                                           var)
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IM
import qualified Data.IntSet              as IS
import           Utils                    (listToVarAssignment,
                                           outputPermutations)

type BDDFun = BDD AscOrder

pick :: ItemOrder o => Int -> BDD o -> BDD o -> BDD o
pick i a0 a1 = ite (var i) a1 a0

bddAsc :: BDD AscOrder -> BDD AscOrder
bddAsc = id

bddDesc :: BDD DescOrder -> BDD DescOrder
bddDesc = id

isConstBDD :: BDD o -> Maybe Bool
isConstBDD (Leaf b) = Just b
isConstBDD _        = Nothing

-- Generating all BDDFuns

allBDDFuns :: ItemOrder o => Int -> [BDD o]
allBDDFuns n = map (bddFromOutput n . listToVarAssignment) $ outputPermutations n

boolToBDD :: Bool -> BDD o
boolToBDD True  = true
boolToBDD False = false

bddFromOutput :: ItemOrder o => Int -> IntMap Bool -> BDD o
bddFromOutput bits = bddFromOutput' bits 1

bddFromOutput' :: ItemOrder o => Int -> Int -> IntMap Bool -> BDD o
bddFromOutput' 0 varN out = boolToBDD (out IM.! varN)
bddFromOutput' bits varN out = pick bits
  (bddFromOutput' (bits - 1) (2 * varN) out)
  (bddFromOutput' (bits - 1) (2 * varN - 1) out)

flipInputs :: BDDFun -> BDDFun
flipInputs bdd = substSet mapping bdd
  where
    vars = IS.toList $ support bdd
    mapping = IM.fromList $ map (\x -> (x, notB (var x))) vars

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
normalizeBDD :: BDD AscOrder -> (BDD AscOrder, Int)
normalizeBDD bdd = (bdd', IS.size vars')
  where
    vars = support bdd
    orderMapping = IM.fromAscList $ zip (IS.toAscList vars) [1 ..]
    (g, n) = toGraph bdd
    g' = IM.map (mapOrder orderMapping) g
    bdd' = fromGraph (g', n)
    vars' = support bdd'

mapOrder :: IntMap Int -> Sig Int -> Sig Int
mapOrder _ s@(SLeaf _)              = s
mapOrder orderMap (SBranch i lo hi) = SBranch (orderMap IM.! i) lo hi

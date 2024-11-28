{-# LANGUAGE FlexibleInstances #-}
module BDD.BDD (
  BDDa,
  isConst,
  pick,
  boolToBDD,
  bddFromOutputVector,
  majFun,
  iteratedFun,
  flipInputs,
  normalizeBDD,
  allNAryBDDs
) where
import           Data.DecisionDiagram.BDD (AscOrder, BDD (Leaf), ItemOrder,
                                           Sig (SBranch, SLeaf), false,
                                           fromGraph, ite, notB, substSet,
                                           support, toGraph, true, var)
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IM
import qualified Data.IntSet              as IS
import           Utils                    (listToVarAssignment,
                                           outputPermutations)

type BDDa = BDD AscOrder

pick :: ItemOrder o => Int -> BDD o -> BDD o -> BDD o
pick i a0 a1 = ite (var i) a1 a0

isConst :: BDD o -> Maybe Bool
isConst (Leaf b) = Just b
isConst _        = Nothing

-- Generating BDDs for all n-ary functions

allNAryBDDs :: ItemOrder o => Int -> [BDD o]
allNAryBDDs n = map (bddFromOutputVector n . listToVarAssignment) $ outputPermutations n

boolToBDD :: Bool -> BDD o
boolToBDD True  = true
boolToBDD False = false

bddFromOutputVector :: ItemOrder o => Int -> IntMap Bool -> BDD o
bddFromOutputVector bits = bddFromOutputVector' bits 1

bddFromOutputVector' :: ItemOrder o => Int -> Int -> IntMap Bool -> BDD o
bddFromOutputVector' 0 varN out = boolToBDD (out IM.! varN)
bddFromOutputVector' bits varN out = pick bits
  (bddFromOutputVector' (bits - 1) (2 * varN) out)
  (bddFromOutputVector' (bits - 1) (2 * varN - 1) out)

flipInputs :: ItemOrder o => BDD o -> BDD o
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
normalizeBDD :: BDD AscOrder -> (BDD AscOrder, Int)
normalizeBDD bdd
  | all (uncurry (==)) mapList = (bdd, nVars)
  | otherwise = (bdd', nVars)
  where
    vars = IS.toAscList $ support bdd
    mapList = zip vars [1 ..]
    nVars = length mapList
    orderMapping = IM.fromAscList mapList
    (g, n) = toGraph bdd
    g' = IM.map (mapOrder orderMapping) g
    bdd' = fromGraph (g', n)

mapOrder :: IntMap Int -> Sig Int -> Sig Int
mapOrder _ s@(SLeaf _)              = s
mapOrder orderMap (SBranch i lo hi) = SBranch (orderMap IM.! i) lo hi

------------- Examples ---------------------------

-- n is the number of bits of the functions
majFun :: Int -> BDDa
majFun n = thresholdBDD threshold 1 n
  where
    threshold = (n `div` 2) + 1

thresholdBDD :: Int -> Int -> Int -> BDDa
thresholdBDD 0 _ _ = true
thresholdBDD threshold i n
  | i > n = false
  | otherwise = pick i
    (thresholdBDD threshold (i + 1) n)
    (thresholdBDD (threshold - 1) (i + 1) n)

iteratedFun :: Int -> Int -> BDDa -> BDDa
iteratedFun bits levels = iteratedFun' bits levels 1

iteratedFun' :: Int -> Int -> Int -> BDDa -> BDDa
iteratedFun' bits _levels _varN f = go (_levels - 1) _varN
  where
    go :: Int -> Int -> BDDa
    go levels varN
      | levels == 0 = substituteBase f bits varN
      | otherwise = substituteSubFuns f subFuns
      where
        varFactor = bits * levels
        factors = iterate (+ varFactor) varN
        subFuns = map (go (levels - 1)) $ take bits factors

substituteBase :: BDDa -> Int -> Int -> BDDa
substituteBase f bits varN = substSet (IM.fromList $ zip [1 .. bits] vars) f
  where
    vars = map var [varN ..]

substituteSubFuns :: BDDa -> [BDDa] -> BDDa
substituteSubFuns f subFuns = substSet (IM.fromList $ zip [1 ..] subFuns) f

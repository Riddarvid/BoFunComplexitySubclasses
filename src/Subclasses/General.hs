module Subclasses.General (
  majFun,
  iteratedFun,
  iteratedMajFun
) where
import           BDD                      (BDDFun, pick)
import           Data.DecisionDiagram.BDD (false, substSet, true, var)
import qualified Data.IntMap              as IM

majFun :: Int -> BDDFun
majFun n = thresholdBDD threshold 0 n
  where
    threshold = (n `div` 2) + 1

thresholdBDD :: Int -> Int -> Int -> BDDFun
thresholdBDD 0 _ _ = true
thresholdBDD threshold i n
  | i >= n = false
  | otherwise = pick i
    (thresholdBDD threshold (i + 1) n)
    (thresholdBDD (threshold - 1) (i + 1) n)

iteratedFun :: Int -> Int -> Int -> BDDFun -> BDDFun
iteratedFun bits _levels _varN f = go (_levels - 1) _varN
  where
    go :: Int -> Int -> BDDFun
    go levels varN
      | levels == 0 = substituteBase f bits varN
      | otherwise = substituteSubFuns f subFuns
      where
        varFactor = bits * levels
        factors = iterate (+ varFactor) varN
        subFuns = map (go (levels - 1)) $ take bits factors

substituteBase :: BDDFun -> Int -> Int -> BDDFun
substituteBase f bits varN = substSet (IM.fromList $ zip [0 .. bits - 1] vars) f
  where
    vars = map var [varN ..]

substituteSubFuns :: BDDFun -> [BDDFun] -> BDDFun
substituteSubFuns f subFuns = substSet (IM.fromList $ zip [0 ..] subFuns) f

iteratedMajFun :: Int -> Int -> BDDFun
iteratedMajFun bits levels = iteratedFun bits levels 0 (majFun bits)

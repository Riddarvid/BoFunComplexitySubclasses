module Subclasses.General (
  majGeneral
) where
import           BDD                      (BDDFun, pick)
import           Data.DecisionDiagram.BDD (ItemOrder, false, true)

majGeneral :: ItemOrder a => Int -> BDDFun a
majGeneral n = thresholdBDD threshold 1 n
  where
    threshold = (n `div` 2) + 1

thresholdBDD :: ItemOrder a => Int -> Int -> Int -> BDDFun a
thresholdBDD 0 _ _ = true
thresholdBDD threshold i n
  | i > n = false
  | otherwise = pick i
    (thresholdBDD threshold (i + 1) n)
    (thresholdBDD (threshold - 1) (i + 1) n)

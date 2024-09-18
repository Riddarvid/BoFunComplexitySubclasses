module Subclasses.GeneralBDD (majBDD) where
import           BDD                      (pick)
import           Data.DecisionDiagram.BDD (BDD, ItemOrder, false, true)

majBDD :: ItemOrder a => Int -> BDD a
majBDD n = thresholdBDD threshold 1 n
  where
    threshold = (n `div` 2) + 1

thresholdBDD :: ItemOrder a => Int -> Int -> Int -> BDD a
thresholdBDD 0 _ _ = true
thresholdBDD threshold i n
  | i > n = false
  | otherwise = pick i
    (thresholdBDD threshold (i + 1) n)
    (thresholdBDD (threshold - 1) (i + 1) n)

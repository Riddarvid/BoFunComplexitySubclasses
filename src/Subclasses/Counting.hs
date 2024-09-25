module Subclasses.Counting (
  numberOfIteratedThresholdFuns,
  allIteratedThresholdFuns
) where
import           Control.Monad.Free   (Free (Free, Pure))
import qualified Data.HashSet         as HS
import qualified Data.MultiSet        as MultiSet
import           DSLsofMath.Algebra   (Additive ((+)))
import           Prelude              hiding ((*), (+))
import           Subclasses.Id        ()
import           Subclasses.Iterated  (Iterated)
import           Subclasses.Threshold (Threshold (Threshold),
                                       ThresholdFun (ThresholdFun),
                                       iteratedThresholdFunConst, toBDD)

-- Ensures that only unique functions are counted by converting them to BDDs
-- and inserting them into a set.
numberOfIteratedThresholdFuns :: Integer -> Int
numberOfIteratedThresholdFuns = HS.size . HS.fromList . map toBDD . allIteratedThresholdFuns

-- This code generates all the iterated threshold functions of a given arity,
-- but there is no guarantee that the functions are unique.
allIteratedThresholdFuns :: Integer -> [Iterated ThresholdFun]
-- 2 constant values exist
allIteratedThresholdFuns 0 = [
  iteratedThresholdFunConst False,
  iteratedThresholdFunConst True]
-- The logic for n == 1 is that we cannot express the function "not" as an iterated threshold fun.
-- Therefore, we can only choose the id function. We can only choose a single threshold
-- as well, resulting in 1 * 1 = 1 iterated threshold functions with 1 bit.
allIteratedThresholdFuns 1 = [Pure ()]
allIteratedThresholdFuns n = allIteratedThresholdFuns' n

type Partition = [Integer]

allIteratedThresholdFuns' :: Integer -> [Iterated ThresholdFun]
allIteratedThresholdFuns' n = concatMap allIteratedThresholdFuns'' $ partitions n

allIteratedThresholdFuns'' :: Partition -> [Iterated ThresholdFun]
allIteratedThresholdFuns'' p = do
  threshold' <- [Threshold (n', n - n' + 1) | n' <- [1 .. n]]
  subFuns <- mapM allIteratedThresholdFuns p
  let subFuns' = MultiSet.fromList subFuns
  return $ Free $ ThresholdFun threshold' subFuns'
  where
    n = length p

-- The reason that we're dropping the last one is that
-- f == ThresholdFun (1,1) [f]. We are not creating a new function by simply wrapping it
-- in a ThresholdFun.
partitions :: Integer -> [Partition]
partitions n = case partitions' 1 n of
  [] -> []
  xs -> init xs

-- Generate all partitions of n where a partition is a sorted list of numbers summing to n.
-- The first element in the partition must be >= highest.
partitions' :: Integer -> Integer -> [Partition]
partitions' highest n
  | n == 0 = [[]]
  | highest > n = []
  | otherwise = concatMap (\m -> map (m :) $ partitions' m (n - m)) [highest .. n]

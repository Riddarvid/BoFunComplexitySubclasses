module Subclasses.Counting (
  numberOfIteratedThresholdFuns,
  allIteratedThresholdFuns,
  allIteratedThresholdFunsMemo
) where
import           Control.Monad.Free    (Free (Free, Pure))
import           Data.Function.Memoize (Memoizable (memoize))
import qualified Data.MultiSet         as MultiSet
import qualified Data.Set              as Set
import           DSLsofMath.Algebra    (Additive ((+)))
import           Prelude               hiding ((*), (+))
import           Subclasses.General    (GenFun, toGenFun)
import           Subclasses.Id         ()
import           Subclasses.Iterated   (Iterated)
import           Subclasses.Threshold  (Partition, Threshold (Threshold),
                                        ThresholdFun (ThresholdFun),
                                        iteratedThresholdFunConst, partitions)

-- Ensures that only unique functions are counted by converting them to BDDs
-- and inserting them into a set.
numberOfIteratedThresholdFuns :: Int -> Int
numberOfIteratedThresholdFuns n = Set.size $ Set.fromList funs
  where
    funs :: [GenFun]
    funs = map (toGenFun n) $ allIteratedThresholdFuns n

allIteratedThresholdFunsMemo :: Int -> [Iterated ThresholdFun]
allIteratedThresholdFunsMemo = memoize allIteratedThresholdFuns

-- This code generates all the iterated threshold functions of a given arity,
-- but there is no guarantee that the functions are unique.
allIteratedThresholdFuns :: Int -> [Iterated ThresholdFun]
-- 2 constant values exist
allIteratedThresholdFuns 0 = [
  iteratedThresholdFunConst False,
  iteratedThresholdFunConst True]
-- The logic for n == 1 is that we cannot express the function "not" as an iterated threshold fun.
-- Therefore, we can only choose the id function. We can only choose a single threshold
-- as well, resulting in 1 * 1 = 1 iterated threshold functions with 1 bit.
allIteratedThresholdFuns 1 = [Pure ()]
allIteratedThresholdFuns n = allIteratedThresholdFuns' n

allIteratedThresholdFuns' :: Int -> [Iterated ThresholdFun]
allIteratedThresholdFuns' n = concatMap allIteratedThresholdFuns'' $ partitions n

allIteratedThresholdFuns'' :: Partition -> [Iterated ThresholdFun]
allIteratedThresholdFuns'' p = do
  threshold' <- [Threshold (n', n - n' + 1) | n' <- [1 .. n]]
  subFuns <- mapM allIteratedThresholdFunsMemo p
  let subFuns' = MultiSet.fromList subFuns
  return $ Free $ ThresholdFun threshold' subFuns'
  where
    n = length p


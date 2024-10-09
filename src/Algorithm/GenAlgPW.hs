{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE BlockArguments #-}
module Algorithm.GenAlgPW (
  computeMin,
  computeMinStep,
  computeMin'
) where
import           BoFun                 (BoFun (..))
import           Control.Arrow         ((>>>))
import           Control.Monad.State   (MonadState (get), State, evalState,
                                        forM, modify)
import           Data.Function         (fix)
import           Data.Function.Memoize (Memoizable (memoize))
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (isJust)
import           Data.Monoid           (Endo (Endo, appEndo))
import           DSLsofMath.Algebra    (Additive (..),
                                        Multiplicative (one, (*)), (-))
import           Poly.PiecewisePoly    (PiecewisePoly, minPWs)
import           Prelude               hiding ((*), (+), (-))

computeMinStep :: (BoFun f i) => Endo (f -> PiecewisePoly Rational)
computeMinStep = Endo $ \recCall fun -> if isJust (isConst fun)
  then zero -- If the function is constant, then it takes 0 steps to calculate it.
  else one + minPWs $ do -- Else, we need one more step to evaluate the next bit, plus some more
    i <- variables fun -- for each unevaluated var
    let
      [a, b] = do
        (value, factor) <- [(False, one - mempty), (True, mempty)] -- represents choosing 0 or 1
        return $ factor * recCall (setBitAndNormalize (i, value) fun)
    return $ a + b

computeMin :: (BoFun f i, Memoizable f) => f -> PiecewisePoly Rational
computeMin = fix $ appEndo computeMinStep >>> memoize

------------------------------------------------------------------------------------

type Complexity = PiecewisePoly Rational

type ComputeState f = Map f Complexity

type ComputeAction f = State (ComputeState f) Complexity

computeMin' :: (Ord f, BoFun f i) => f -> Complexity
computeMin' f = evalState (computeMin'' f) Map.empty

computeMin'' :: (Ord f, BoFun f i) => f -> ComputeAction f
computeMin'' f = case isConst f of
  Just _ -> return zero
  Nothing -> do
    memoMap <- get
    case Map.lookup f memoMap of
      Just c  -> return c
      Nothing -> do
        subComplexities' <- subComplexities f
        let c = one + minPWs subComplexities'
        modify (Map.insert f c)
        return c

subComplexities :: (BoFun f i, Ord f) => f -> State (ComputeState f) [Complexity]
subComplexities f = forM vars (subComplexity f)
  where
    vars = variables f

subComplexity :: (Ord f, BoFun f i) => f -> i -> ComputeAction f
subComplexity f i = do
  a <- subComplexity' f i (False, one - mempty)
  b <- subComplexity' f i (True, mempty)
  return $ a + b

subComplexity' :: (Ord f, BoFun f i) => f -> i -> (Bool, Complexity) -> ComputeAction f
subComplexity' f i (v, factor) = do
  c <- computeMin'' (setBitAndNormalize (i, v) f)
  return $ factor * c

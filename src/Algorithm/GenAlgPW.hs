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
import           Data.Hashable         (Hashable)
import           Data.HashMap.Lazy     (HashMap)
import qualified Data.HashMap.Lazy     as HM
import           Data.Maybe            (isJust)
import           Data.Monoid           (Endo (Endo, appEndo))
import           Data.Ratio            ((%))
import           DSLsofMath.Algebra    (Additive (..),
                                        Multiplicative (one, (*)), (-))
import           Poly.PiecewisePoly    (PiecewisePoly, minPWs, mirrorPW)
import           Prelude               hiding ((*), (+), (-))

computeMinStep :: (BoFun f i) => Endo (f -> PiecewisePoly Rational)
computeMinStep = Endo $ \recCall fun -> if isJust (isConst fun)
  then zero -- If the function is constant, then it takes 0 steps to calculate it.
  else one + minPWs $ do -- Else, we need one more step to evaluate the next bit, plus some more
    i <- variables fun -- for each unevaluated var
    let
      [a, b] = do
        (value, factor) <- [(False, one - mempty), (True, mempty)] -- represents choosing 0 or 1
        return $ factor * recCall (setBit (i, value) fun)
    return $ a + b

computeMin :: (BoFun f i, Memoizable f) => f -> PiecewisePoly Rational
computeMin = fix $ appEndo computeMinStep >>> memoize

------------------------------------------------------------------------------------

type Complexity = PiecewisePoly Rational

type ComputeState f = HashMap f [(f, Complexity)]

type ComputeAction f = State (ComputeState f) Complexity

lookupList :: Hashable a => a -> HashMap a [(a, b)] -> Maybe b
lookupList f compMap = do
  vals <- HM.lookup f compMap
  lookup f vals

insertList :: Hashable a => a -> b -> HashMap a [(a, b)] -> HashMap a [(a, b)]
insertList f v = HM.insertWith (++) f [(f, v)]

-- Saved in case we want to explore mirrored complexities further.
{-insertListMirror :: (Hashable f, BoFun f i) =>
  f -> Complexity -> HashMap f [(f, Complexity)] -> HashMap f [(f, Complexity)]
insertListMirror f c = insertList f' c' . insertList f c
  where
    f' = flipInputs f
    c' = mirrorPW (1 % 2) c-}


computeMin' :: (BoFun f i, Hashable f) => f -> Complexity
computeMin' f = evalState (computeMin'' f) HM.empty

computeMin'' :: (BoFun f i, Hashable f) => f -> ComputeAction f
computeMin'' f = case isConst f of
  Just _ -> return zero
  Nothing -> do
    memoMap <- get
    case lookupList f memoMap of
      Just c  -> return c
      Nothing -> do
        subComplexities' <- subComplexities f
        let c = one + minPWs subComplexities'
        modify (insertList f c)
        return c

subComplexities :: (BoFun f i, Hashable f) => f -> State (ComputeState f) [Complexity]
subComplexities f = forM vars (subComplexity f)
  where
    vars = variables f

subComplexity :: (BoFun f i, Hashable f) =>f -> i -> ComputeAction f
subComplexity f i = do
  a <- subComplexity' f i (False, one - mempty)
  b <- subComplexity' f i (True, mempty)
  return $ a + b

subComplexity' :: (BoFun f i, Hashable f) => f -> i -> (Bool, Complexity) -> ComputeAction f
subComplexity' f i (v, factor) = do
  c <- computeMin'' (setBit (i, v) f)
  return $ factor * c

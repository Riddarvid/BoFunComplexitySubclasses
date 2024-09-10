{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Algorithm.GenAlgPW (
  computeMin
) where
import           BoFun                 (BoFun (..))
import           Control.Arrow         ((>>>))
import           Data.Function         (fix)
import           Data.Function.Memoize (Memoizable (memoize))
import           Data.Maybe            (isJust)
import           Data.Monoid           (Endo (Endo, appEndo))
import           DSLsofMath.Algebra    (Additive (..),
                                        Multiplicative (one, (*)), (-))
import           Poly.PiecewisePoly    (PiecewisePoly, minPWs)
import           Prelude               hiding ((*), (+), (-))

computeMinStep :: (BoFun f i) =>Endo (f -> PiecewisePoly Rational)
computeMinStep = Endo $ \recCall fun -> if isJust (isConst fun)
  then zero -- If the function is constant, then it takes 0 steps to calculate it.
  else one + minPWs $ do -- Else, we need one more step to evaluate the next bit, plus some more
    i <- variables fun -- for each unevaluated var
    let
      [a, b] = do
        (value, factor) <- [(False, mempty), (True, one - mempty)] -- represents choosing 0 or 1
        return $ factor * recCall (setBit (i, value) fun)
    return $ a + b

computeMin :: (BoFun f i, Memoizable f) =>f -> PiecewisePoly Rational
computeMin = fix $ appEndo computeMinStep >>> memoize


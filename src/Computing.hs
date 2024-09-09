module Computing (computeMin) where
import           BoFun                 (BoFun (..))
import           Control.Arrow         ((>>>))
import           Data.Function         (fix)
import           Data.Function.Memoize (Memoizable (..), memoFix, traceMemoize)
import           Data.Maybe            (isJust)
import           Data.Monoid           (Endo (..))
import           DSLsofMath.Algebra    (Additive (zero, (+)),
                                        Multiplicative (one, (*)), (-))
import           PiecewisePoly         (PiecewisePoly, minPWs)
import           Prelude               hiding ((*), (+), (-))

-- TODO: Would be nice to create a "both" function similar to the one in All, so that
-- we could know which function resulted in a certain piece of a PW.
computeMinStep :: (Show f, BoFun f i) => Endo (f -> PiecewisePoly Rational)
computeMinStep = Endo $ \recCall fun -> if isJust (isConst fun)
  then zero -- If the function is constant, then it takes 0 steps to calculate it.
  else one + minPWs $ do -- Else, we need one more step to evaluate the next bit, plus some more
    i <- variables fun -- for each unevaluated var
    let
      [a, b] = do
        (value, factor) <- [(False, mempty), (True, one - mempty)] -- represents choosing 0 or 1
        return $ factor * recCall (setBit (i, value) fun)
    return $ a + b
-- The do block returns a list of Piecewise Polys. Each element represents choosing a certain bit.
-- For each element, the polynomial is the result of choosing either 0 or 1.
-- On line 22, we calculate the expected value given that we evaluate a certain bit.
-- That is, prob of 1 * cost if 1 + prob of 0 * cost if 0.
-- So the do block returns a list of expected values.
-- We still need to figure out what exactly minPWs does.
-- I guess it returns the piecewise polynomial representing the cheapest "pieces"
-- for the various ranges.
-- QUESTION: Check that our explanation/understanding is correct.
-- Syfte med Endo: Göra mer kompatibelt med Agda, representerar en rekursion

-- QUESTION: What are the minimum requirements to be able to use computeMin?
-- Which typeclasses do we have to implement? -- Känns som att vi förstår detta nu.
computeMin :: (Show f, BoFun f i, Memoizable f) => f -> PiecewisePoly Rational
computeMin = fix $ appEndo computeMinStep >>> memoize

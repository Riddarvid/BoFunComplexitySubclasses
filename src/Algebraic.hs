{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant guard" #-}
module Algebraic (
  Algebraic(Algebraic),
  fromPWAlgebraic,
  toAlgebraic,
  shrinkIntervalStep
) where
import           Control.Monad      (replicateM)
import           Data.List          (nub, sort)
import           Data.Ratio         ((%))
import           DSLsofMath.Algebra (AddGroup (..), Additive (..),
                                     MulGroup ((/)), Multiplicative (..),
                                     product, (-))
import           DSLsofMath.PSDS    (Poly (P))
import           Poly.Utils         (numRootsInInterval, removeDoubleRoots)
import           Prelude            hiding (negate, product, sum, (*), (+), (-),
                                     (/))
import           Test.QuickCheck    (Arbitrary (arbitrary), Gen, chooseInt)

data Algebraic = Algebraic (Poly Rational) (Rational, Rational)
  deriving (Show)

------------------ QuickCheck -------------------------------------------------

instance Arbitrary Algebraic where
  arbitrary :: Gen Algebraic
  arbitrary = do
    n <- chooseInt (1, 2)
    (polys, roots) <- unzip <$> replicateM n genRootPoly
    let poly = removeDoubleRoots $ product polys
    int <- chooseInterval $ nub $ sort roots
    return $ Algebraic poly int

-- roots must be sorted and nubbed
chooseInterval :: [Rational] -> Gen (Rational, Rational)
chooseInterval roots = do
  i <- chooseInt (0, length roots - 1)
  let root = roots !! i
  let leftRoot = if i == 0 then root - 100 else roots !! (i - 1)
  let rightRoot = if i == length roots - 1 then root + 100 else roots !! (i + 1)
  let low = leftRoot + (root - leftRoot) * (1 % 10)
  let high = rightRoot - (rightRoot - root) * (1 % 10)
  return (low, high)

-- Generates a first degree polynomial with a root in n
genRootPoly :: (Arbitrary a, AddGroup a, Multiplicative a) => Gen (Poly a, a)
genRootPoly = do
  n <- arbitrary
  return (P [negate n, one], n)

-------------------------- Conversions to/from algebraic -------------------------

fromPWAlgebraic :: (Real a) => (Poly a, (a, a)) -> Algebraic
fromPWAlgebraic (p, (low, high)) = Algebraic (fmap toRational p) (toRational low, toRational high)

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (x' - 1, x' + 1) which is guaranteed to contain x'
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = Algebraic (P [negate x', one]) (x' - one, x' + one)
  where
    x' = toRational x

------------ Interval shrinking --------------------------------------------------

-- Is not used anywhere right now but useful for approximating an algebraic number
-- with arbitrary precision.
-- An alternative could be using the code from Filter, in the function
-- shrinkIntervalPoly, which works on the assumption that low <= 0, high >= 0.
-- Shrinks the interval by dividing it into two pieces and checking which
-- piece contains the root.
-- TODO-NEW quickCheck
shrinkIntervalStep :: Algebraic -> Algebraic
shrinkIntervalStep (Algebraic p (low, high))
  | nRoots > 1 || nRoots < 0 = error "Interval has too many roots"
  |otherwise = case rootsLeft of
  0 -> Algebraic p (mid, high)
  1 -> Algebraic p (low, mid)
  _ -> error "Original interval should contain exactly one root"
  where
    mid = (low + high) / 2
    rootsLeft = numRootsInInterval p (low, mid)
    rootsRight = numRootsInInterval p (mid, high)
    nRoots = rootsLeft + rootsRight

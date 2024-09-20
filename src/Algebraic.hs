{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant guard" #-}
module Algebraic (
  Algebraic(Algebraic),
  fromPWAlgebraic,
  toAlgebraic,
  Sign(..),
  signAt,
  shrinkInterval,
  hasMaximum,
  countMaxima
) where
import           Control.Monad      (replicateM)
import           Data.List          (nub, sort)
import           Data.Ratio         ((%))
import           DSLsofMath.Algebra (AddGroup (..), Additive (..),
                                     MulGroup ((/)), Multiplicative (..),
                                     product, (-))
import           DSLsofMath.PSDS    (Poly (P), derP, evalP, gcdP, yun)
import           Poly.PiecewisePoly (PiecewisePoly, Separation, linearizePW)
import qualified Poly.PiecewisePoly as PW
import           Poly.Utils         (numRootsInInterval, removeDoubleRoots)
import           Prelude            hiding (negate, product, sum, (*), (+), (-),
                                     (/))
import           Test.QuickCheck    (Arbitrary (arbitrary), Gen, chooseInt)

data Algebraic = Algebraic (Poly Rational) (Rational, Rational)
  deriving (Show)

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

genRootPoly :: (Arbitrary a, AddGroup a, Multiplicative a) => Gen (Poly a, a)
genRootPoly = do
  n <- arbitrary
  return (P [negate n, one], n)


fromPWAlgebraic :: (Real a) => (Poly a, (a, a)) -> Algebraic
fromPWAlgebraic (p, (low, high)) = Algebraic (fmap toRational p) (toRational low, toRational high)

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (x' - 1, x' + 1) which is guaranteed to contain x'
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = Algebraic (P [negate x', one]) (x' - one, x' + one)
  where
    x' = toRational x

------------ Double root removal ---------------------------



------------ Interval shrinking ----------------------------

-- Shrink the interval by dividing it into two pieces and checking which
-- one has a root.
-- TODO kolla på inclusive/exclusive
-- quickCheck
shrinkInterval :: Algebraic -> Algebraic
shrinkInterval (Algebraic p (low, high))
  | nRoots > 1 || nRoots < 0 = error "Interval has too many roots"
  |otherwise = case numRootsInInterval p (low, mid) of
  0 -> Algebraic p (mid, high)
  1 -> Algebraic p (low, mid)
  _ -> error "Original interval should contain exactly one root"
  where
    mid = (low + high) / 2
    rootsLeft = numRootsInInterval p (low, mid)
    rootsRight = numRootsInInterval p (mid, high)
    nRoots = rootsLeft + rootsRight

data Sign = Neg | Zero | Pos
  deriving (Eq)

signAt :: Algebraic -> Poly Rational -> Sign
signAt n q
  | shareRoot n q = Zero
  | otherwise = signAt' (normalizeLimits n) q

-- Assumes that p and q do not share a root in the interval and that p(l) < 0 and p(h) > 0
signAt' :: Algebraic -> Poly Rational -> Sign
signAt' n@(Algebraic _ int@(l, _)) q
  | numRootsInInterval q int == 0 = signAtDyadic l q
  | otherwise = signAt' (shrinkIntervalPoly n) q

shrinkIntervalPoly :: Algebraic -> Algebraic
shrinkIntervalPoly (Algebraic p (l, h)) = case signAtDyadic mid p of
  Neg -> Algebraic p (mid, h)
  _   -> Algebraic p (l, mid)
  where
    mid = (l + h) / 2

signAtDyadic :: (Ord a, AddGroup a, Multiplicative a) => a -> Poly a -> Sign
signAtDyadic x p = case compare res zero of
  LT -> Neg
  EQ -> Zero
  GT -> Pos
  where
    res = evalP p x

normalizeLimits :: Algebraic -> Algebraic
normalizeLimits n@(Algebraic p (l, h))
  | evalP p l < 0 = n
  | otherwise = Algebraic (negate p) (l, h)

shareRoot :: Algebraic -> Poly Rational -> Bool
shareRoot (Algebraic p int) q = numRootsInInterval r int > 0
  where
    r = gcdP p q

-- TODO maybe better naming of variables?
-- The problem right now is finding the correct root.
-- Another problem is that the root should be a real number but currently
-- we're casting it to a rational number, which of course changes the value.
-- Really, we should make the function work with real numbers instead.
hasMaximum :: (Real a, AddGroup a, Multiplicative a) => Poly a -> Poly a -> Separation a -> Bool
hasMaximum p1 p2 s = case s of
  PW.Dyadic x    -> evalP p1' x > zero && evalP p2' x < zero
  PW.Algebraic x -> let
    x' = fromPWAlgebraic x
    s1 = signAt x' (fmap toRational p1')
    s2 = signAt x' (fmap toRational p2')
    in s1 == Pos && s2 == Neg
  where
    p1' = derP p1
    p2' = derP p2

countMaxima :: (Real a, AddGroup a, MulGroup a) => PiecewisePoly a -> Int
countMaxima pw = countMaxima' $ linearizePW pw

countMaxima' :: (AddGroup a, Multiplicative a, Real a) => [Either (Poly a) (Separation a)] -> Int
countMaxima' (Right _ : xs) = countMaxima' xs
countMaxima' (Left p1 : Right s : Left p2 : xs)
  | hasMaximum p1 p2 s = 1 + rest
  | otherwise = rest
  where
    rest = countMaxima' (Left p2 : xs)
countMaxima' _ = 0

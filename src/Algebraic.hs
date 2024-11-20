{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant guard" #-}
module Algebraic (
  Algebraic(..),
  AlgRep(..),
  fromPWAlgebraic,
  fromPWSeparation,
  toPWAlgebraic,
  toAlgebraic,
  shrinkIntervalStep,
  signAtAlgebraic,
  signAtRational
) where
import           Control.Monad      (replicateM)
import           Data.List          (nub, sort)
import           Data.Ratio         ((%))
import           DSLsofMath.Algebra (AddGroup (..), Additive (..),
                                     MulGroup ((/)), Multiplicative (..),
                                     product, two, (-))
import           DSLsofMath.PSDS    (Poly (P), evalP, gcdP)
import           Poly.PiecewisePoly (Separation)
import qualified Poly.PiecewisePoly as PW
import           Poly.Utils         (isRoot, numRootsInInterval,
                                     removeDoubleRoots)
import           Prelude            hiding (negate, product, sum, (*), (+), (-),
                                     (/))
import           Test.QuickCheck    (Arbitrary (arbitrary), Gen, chooseInt)
import           Utils              (Sign (..))

data Algebraic = Algebraic AlgRep | Rational Rational

-- The interval is (a, b)
data AlgRep = AlgRep (Poly Rational) (Rational, Rational)
  deriving (Show)

instance Eq Algebraic where
  (==) :: Algebraic -> Algebraic -> Bool
  (==) = undefined
{-instance Eq Algebraic where
  (==) :: Algebraic -> Algebraic -> Bool
  (AlgRep p i1@(lowP, highP)) == (AlgRep q i2@(lowQ, highQ))
    | lowP <= lowQ && highQ <= highP = numRootsInInterval r i2 == 1
    | lowQ <= lowP && highP <= highQ = numRootsInInterval r i1 == 1
    | otherwise = False
    where
      r = gcdP p q-}

-- 1) Shrink until either the two intervals are distinct, or one completely
-- contains the other.
-- 2) If one completetly contains the other, check if they are equal. If so, we are done.
-- 3) Otherwise, compare the distinct intervals.
instance Ord Algebraic where
  compare :: Algebraic -> Algebraic -> Ordering
  compare a b = case compareDistinct a b of
    Nothing  -> compare (shrinkIntervalStep a) (shrinkIntervalStep b)
    Just res -> res

compareDistinct = undefined
{-
compareDistinct :: Algebraic -> Algebraic -> Maybe Ordering
compareDistinct a@(AlgRep _ (lowP, highP)) b@(AlgRep _ (lowQ, highQ))
  | highP <= lowQ = Just LT
  | highQ <= lowP = Just GT
  | a == b = Just EQ
  | otherwise = Nothing-}

------------------ QuickCheck -------------------------------------------------

instance Arbitrary AlgRep where
  arbitrary :: Gen AlgRep
  arbitrary = do
    n <- chooseInt (1, 2)
    (polys, roots) <- unzip <$> replicateM n genRootPoly
    let poly = removeDoubleRoots $ product polys
    int <- chooseInterval $ nub $ sort roots
    return $ AlgRep poly int

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

fromPWAlgebraic :: (Poly Rational, (Rational, Rational)) -> AlgRep
fromPWAlgebraic (p, (low, high)) = AlgRep p (low, high)

fromPWSeparation :: Separation Rational -> Algebraic
fromPWSeparation (PW.Dyadic r)    = Rational r
fromPWSeparation (PW.Algebraic a) = Algebraic $ fromPWAlgebraic a

toPWAlgebraic :: Algebraic -> Separation Rational
toPWAlgebraic (Rational r) = PW.Algebraic (p, (low, high))
  where
    AlgRep p (low, high) = toAlgRep r
toPWAlgebraic (Algebraic (AlgRep p interval)) = PW.Algebraic (p, interval)

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (x' - 1, x' + 1) which is guaranteed to contain x'
toAlgebraic :: Rational -> Algebraic
toAlgebraic = Algebraic . toAlgRep

toAlgRep :: Rational -> AlgRep
toAlgRep x = AlgRep (P [negate x, one]) (x - one, x + one)

------------ Interval shrinking --------------------------------------------------

shrinkIntervalStep :: Algebraic -> Algebraic
shrinkIntervalStep (Rational r)  = Rational r
shrinkIntervalStep (Algebraic a) = shrinkIntervalStep' a

shrinkIntervalStep' :: AlgRep -> Algebraic
shrinkIntervalStep' (AlgRep p (low, high))
  | isRoot mid p = Rational mid
  |otherwise = case rootsLeft of
    0 -> Algebraic $ AlgRep p (mid, high)
    1 -> Algebraic $ AlgRep p (low, mid)
    _ -> error "Too many roots"
  where
    mid = (low + high) / 2
    rootsLeft = numRootsInInterval p (low, mid)

---------------- Sign checking -----------------------------

signAtAlgebraic :: Algebraic -> Poly Rational -> Sign
signAtAlgebraic a@(Rational _) p = signAtAlgebraic' a p
signAtAlgebraic (Algebraic a) q
  | shareRoot a q = Zero
  | otherwise = signAtAlgebraic' (Algebraic $ normalizeLimits a) q

shareRoot :: AlgRep -> Poly Rational -> Bool
shareRoot (AlgRep p int) q = numRootsInInterval r int > 0
  where
    r = gcdP p q

normalizeLimits :: AlgRep -> AlgRep
normalizeLimits n@(AlgRep p (l, h))
  | evalP p l > 0 || evalP p h < 0 = AlgRep (negate p) (l, h)
  | otherwise = n

-- Assumes that p and q do not share a root in the interval and that p(l) <= 0 and p(h) >= 0
signAtAlgebraic' :: Algebraic -> Poly Rational -> Sign
signAtAlgebraic' (Rational r)  = signAtRational r
signAtAlgebraic' (Algebraic a) = signAtAlgebraic'' a

signAtAlgebraic'' :: AlgRep -> Poly Rational -> Sign
signAtAlgebraic'' n@(AlgRep _ interval) q
  | numRootsInInterval q interval == 0 = signAtRational (intMid interval) q
  | otherwise = signAtAlgebraic' (shrinkIntervalPoly' n) q

signAtRational :: (Ord a, AddGroup a, Multiplicative a) => a -> Poly a -> Sign
signAtRational x p = case compare res zero of
  LT -> Neg
  EQ -> Zero
  GT -> Pos
  where
    res = evalP p x

-- Only works if p(l) < 0 and p(h) > 0
-- In other cases, use shrinkIntervalStep for general algebraic numbers.
shrinkIntervalPoly' :: AlgRep -> Algebraic
shrinkIntervalPoly' (AlgRep p int@(l, h)) = case signAtRational mid p of
  Zero -> Rational mid
  Neg  -> Algebraic $ AlgRep p (mid, h)
  Pos  -> Algebraic $ AlgRep p (l, mid)
  where
    mid = intMid int

------------ Utils ----------------------------

intMid :: (Additive a, MulGroup a) => (a, a) -> a
intMid (l, h) = (l + h) / two

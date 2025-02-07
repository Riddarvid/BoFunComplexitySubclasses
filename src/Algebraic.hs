{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant guard" #-}

-- Algebraic numbers for arbitrary precision
module Algebraic (
  Algebraic(..),
  AlgRep(..),
  fromPWAlgebraic,
  fromPWSeparation,
  toPWAlgebraic,
  toAlgebraic,
  shrinkIntervalStep,
  signAtAlgebraic,
  signAtRational,
  translateRational
) where
import           Data.Maybe           (catMaybes)
import           DSLsofMath.Algebra   (AddGroup (..), Additive (..),
                                       MulGroup ((/)), Multiplicative (..), two,
                                       (-))
import           DSLsofMath.PSDS      (Poly (P), evalP, gcdP)
import           Poly.PiecewisePoly   (Separation)
import qualified Poly.PiecewisePoly   as PW
import           Poly.PolyInstances   ()
import           Poly.PolynomialExtra (genNonZeroPoly, translateInput)
import           Poly.Utils           (isRoot, numRootsInInterval,
                                       removeDoubleRoots)
import           Prelude              hiding (negate, product, sum, (*), (+),
                                       (-), (/))
import           Test.QuickCheck      (Arbitrary (arbitrary), Gen, oneof)
import           Utils                (Sign (..))

data Algebraic = Algebraic AlgRep | Rational Rational
  deriving (Show)

-- The interval is (a, b)
data AlgRep = AlgRep (Poly Rational) (Rational, Rational)
  deriving (Show)

instance Eq Algebraic where
  (==) :: Algebraic -> Algebraic -> Bool
  (Rational r1) == (Rational r2)           = r1 == r2
  (Rational r) == (Algebraic (AlgRep p (low, high))) = low < r && r < high && evalP p r == 0
  (Algebraic (AlgRep p (low, high))) == (Rational r) = low < r && r < high && evalP p r == 0
  (Algebraic a) == (Algebraic b)           = a == b

instance Eq AlgRep where
  (==) :: AlgRep -> AlgRep -> Bool
  a@(AlgRep p (lowP, highP)) == b@(AlgRep q (lowQ, highQ)) =
    overlap a b && numRootsInInterval r (max lowP lowQ, min highP highQ) == 1
    where
      r = gcdP p q

-- 1) Shrink until either the two intervals are distinct, or one completely
-- contains the other.
-- 2) If one completetly contains the other, check if they are equal. If so, we are done.
-- 3) Otherwise, compare the distinct intervals.
instance Ord Algebraic where
  compare :: Algebraic -> Algebraic -> Ordering
  compare a b
    | a == b = EQ
    | otherwise = compare' a b

instance Ord AlgRep where
  compare :: AlgRep -> AlgRep -> Ordering
  compare a b = compare (Algebraic a) (Algebraic b)

compare' :: Algebraic -> Algebraic -> Ordering
compare' a b = case compareDistinct a b of
  Just res -> res
  Nothing  ->  compare' (shrinkIntervalStep a) (shrinkIntervalStep b)

compareDistinct :: Algebraic -> Algebraic -> Maybe Ordering
compareDistinct (Rational r1) (Rational r2) = Just $ compare r1 r2
compareDistinct (Rational r) (Algebraic (AlgRep _ (low, high)))
  | r <= low  = Just LT
  | r >= high = Just GT
  | otherwise = Nothing
compareDistinct (Algebraic (AlgRep _ (low, high))) (Rational r)
  | r <= low  = Just GT
  | r >= high = Just LT
  | otherwise = Nothing
compareDistinct (Algebraic a) (Algebraic b) = compareDistinctAlgReps a b

compareDistinctAlgReps :: AlgRep -> AlgRep -> Maybe Ordering
compareDistinctAlgReps (AlgRep _ (lowP, highP)) (AlgRep _ (lowQ, highQ))
  | highP <= lowQ = Just LT
  | highQ <= lowP = Just GT
  | otherwise = Nothing

overlap :: AlgRep -> AlgRep -> Bool
overlap a b = case compareDistinctAlgReps a b of
  Nothing -> True
  _       -> False

------------------ QuickCheck -------------------------------------------------

instance Arbitrary Algebraic where
  arbitrary :: Gen Algebraic
  arbitrary = oneof [Algebraic <$> arbitrary, Rational <$> arbitrary]

-- Currently, this instance only generates rational numbers, which is not ideal.
instance Arbitrary AlgRep where
  arbitrary :: Gen AlgRep
  arbitrary = do
    p <- removeDoubleRoots <$> genNonZeroPoly
    case genInterval p of
      Nothing          -> arbitrary
      Just intervalGen -> AlgRep p <$> intervalGen

genInterval :: Poly Rational -> Maybe (Gen (Rational, Rational))
genInterval p = genInterval' p (-999999999, 999999999)

-- The interval must contain at least one root
genInterval' :: Poly Rational -> (Rational, Rational) -> Maybe (Gen (Rational, Rational))
genInterval' p interval@(low, high)
  | nRoots == 0 = Nothing
  | nRoots == 1 = Just $ return interval
  | otherwise = Just $ oneof gens
  where
    gens = catMaybes [leftGen, rightGen]
    leftGen = genInterval' p (low, mid)
    rightGen = genInterval' p (mid, high)
    nRoots = numRootsInInterval p interval
    mid = (low + high) / 2

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

------------ Translation with rational numbers ---------

translateRational :: Algebraic -> Rational -> Algebraic
translateRational a b = case a of
  Rational a' -> Rational (a' + b)
  Algebraic (AlgRep p (low, high)) -> Algebraic $ AlgRep (translateInput (-b) p) (low + b, high + b)

------------ Utils ----------------------------

intMid :: (Additive a, MulGroup a) => (a, a) -> a
intMid (l, h) = (l + h) / two

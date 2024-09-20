{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Filters (
  piecesPred,
  degreePred,
  maximaPred
) where
import           Algebraic          (Algebraic (Algebraic), fromPWAlgebraic)
import           BDD.BDDInstances   ()
import           Control.Arrow      ((>>>))
import           DSLsofMath.Algebra (AddGroup (negate), Additive (zero),
                                     MulGroup, Multiplicative)
import           DSLsofMath.PSDS    (Poly, derP, evalP, gcdP)
import           Poly.PiecewisePoly (PiecewisePoly, Separation, linearizePW)
import qualified Poly.PiecewisePoly as PW
import           Poly.Utils         (countPieces, findDegreePW,
                                     numRootsInInterval)
import           Prelude            hiding (negate)

piecesPred :: (AddGroup a, MulGroup a, Eq a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
piecesPred p = countPieces >>> p

degreePred :: (Eq a, AddGroup a, MulGroup a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
degreePred p = findDegreePW >>> p

---------------------- Maxima finding -----------------------------------------------------

maximaPred :: (Real a, AddGroup a, MulGroup a) => (Int -> Bool) -> (PiecewisePoly a -> Bool)
maximaPred p = countMaxima >>> p

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

-- Checks if two polynomials meet in a maximum point by comparing ther derivatives
  -- in the given point
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

data Sign = Neg | Zero | Pos
  deriving (Eq)

signAt :: Algebraic -> Poly Rational -> Sign
signAt n q
  | shareRoot n q = Zero
  | otherwise = signAt' (normalizeLimits n) q

shareRoot :: Algebraic -> Poly Rational -> Bool
shareRoot (Algebraic p int) q = numRootsInInterval r int > 0
  where
    r = gcdP p q

normalizeLimits :: Algebraic -> Algebraic
normalizeLimits n@(Algebraic p (l, h))
  | evalP p l < 0 = n
  | otherwise = Algebraic (negate p) (l, h)

-- Assumes that p and q do not share a root in the interval and that p(l) < 0 and p(h) > 0
signAt' :: Algebraic -> Poly Rational -> Sign
signAt' n@(Algebraic _ int@(l, _)) q
  | numRootsInInterval q int == 0 = signAtDyadic l q
  | otherwise = signAt' (shrinkIntervalPoly n) q

signAtDyadic :: (Ord a, AddGroup a, Multiplicative a) => a -> Poly a -> Sign
signAtDyadic x p = case compare res zero of
  LT -> Neg
  EQ -> Zero
  GT -> Pos
  where
    res = evalP p x

shrinkIntervalPoly :: Algebraic -> Algebraic
shrinkIntervalPoly (Algebraic p (l, h)) = case signAtDyadic mid p of
  Neg -> Algebraic p (mid, h)
  _   -> Algebraic p (l, mid)
  where
    mid = (l + h) / 2

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Exploration.Filters (
  piecesPred,
  degreePred,
  maximaPred
) where
import           Algebraic          (fromPWAlgebraic, signAtAlgebraic)
import           Control.Arrow      ((>>>))
import           DSLsofMath.Algebra (AddGroup, Additive (zero, (+)), MulGroup,
                                     Multiplicative)
import           DSLsofMath.PSDS    (Poly, derP, evalP)
import           Poly.PiecewisePoly (PiecewisePoly, Separation, linearizePW)
import qualified Poly.PiecewisePoly as PW
import           Poly.PolyInstances ()
import           Poly.Utils         (countPieces, findDegreePW)
import           Prelude            hiding (negate, (+), (/))
import           Utils              (Sign (..))

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
    s1 = signAtAlgebraic x' (fmap toRational p1')
    s2 = signAtAlgebraic x' (fmap toRational p2')
    in s1 == Pos && s2 == Neg
  where
    p1' = derP p1
    p2' = derP p2

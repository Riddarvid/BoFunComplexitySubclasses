module Exploration.Critical (
  Critical(..),
  CriticalPoint,
  findCritcalPointsPoly,
  findCriticalPointSeparation,
  findCritcalPointsPW
) where
import           Algebraic          (AlgRep (AlgRep),
                                     Algebraic (Algebraic, Rational),
                                     fromPWAlgebraic, fromPWSeparation,
                                     shrinkIntervalStep, signAtAlgebraic,
                                     signAtRational, toAlgebraic, toPWAlgebraic)
import           Debug.Trace        (trace, traceShow)
import           DSLsofMath.Algebra (Additive (zero), Multiplicative (one))
import           DSLsofMath.PSDS    (Poly, derP, evalP, isConstP)
import           Poly.PiecewisePoly (PiecewisePoly, Separation,
                                     Separation' (Dyadic), linearizePW, showPW)
import qualified Poly.PiecewisePoly as PW
import           Poly.Utils         (numRootsInInterval)
import           Utils              (Sign (..))

data Critical = Maximum | Minimum | Saddle
  deriving (Show, Eq)

type CriticalPoint = (Algebraic, Critical)

------------ In polynomials ------------------------

findCritcalPointsPoly :: Separation Rational -> Poly Rational -> Separation Rational -> [CriticalPoint]
findCritcalPointsPoly s1 p s2 = findCritcalPointsPoly' r1 p r2
  where
    (r1, r2) = findRationalEndpoints s1 p s2

findRationalEndpoints :: Separation Rational -> Poly Rational -> Separation Rational -> (Rational, Rational)
findRationalEndpoints s1 p s2 = case (s1, s2) of
  (Dyadic r1, Dyadic r2)           -> (r1, r2)
  (Dyadic r1, PW.Algebraic a)      ->
    let a' = fromPWAlgebraic a
    in (r1, findRight' p' r1 a')
  (PW.Algebraic a, Dyadic r2)      ->
    let a' = fromPWAlgebraic a
    in (findLeft' p' r2 a', r2)
  (PW.Algebraic a, PW.Algebraic b) ->
    let a' = fromPWAlgebraic a
        b' = fromPWAlgebraic b
    in findBoth' p' a' b'
  where
    p' = derP p


findRight' :: Poly Rational -> Rational -> Algebraic -> Rational
findRight' _ _ (Rational r) = r
findRight' p' r a@(Algebraic (AlgRep _ (low, _)))
  | low <= r = findRight' p' r $ shrinkIntervalStep a
  | otherwise = findRight p' r a

-- Finds the rational limit to the right having the same number of roots as
-- the limit specified by q.
findRight :: Poly Rational -> Rational -> Algebraic -> Rational
findRight _ _ (Rational r) = r
findRight p' left a@(Algebraic (AlgRep _ (low, high)))
  | signAtAlgebraic a p' == Zero && (numSmall == numLarge - 1) = low
  | numSmall == numLarge = low
  | otherwise = findRight p' left a'
  where
    numSmall = numRootsInInterval p' (left, low)
    numLarge = numRootsInInterval p' (left, high)
    a' = shrinkIntervalStep a

{-
findLeft' :: Poly Rational -> Rational -> Algebraic -> Rational
findLeft' p' r a@(Algebraic _ (_, high))
  | high >= r = findLeft' p' r $ shrinkIntervalStep a
  | otherwise = findLeft p' r a

findLeft :: Poly Rational -> Rational -> Algebraic -> Rational
findLeft p' right a@(Algebraic _ (low, high))
  | signAtAlgebraic a p' == Zero && (numSmall == numLarge - 1) = high
  | numLarge == numSmall = high
  | otherwise = findRight p' right a'
  where
    numLarge = numRootsInInterval p' (low, right)
    numSmall = numRootsInInterval p' (high, right)
    a' = shrinkIntervalStep a

findBoth' :: Poly Rational -> Algebraic -> Algebraic -> (Rational, Rational)
findBoth' p' a@(Algebraic _ (_, leftHigh)) b@(Algebraic _ (rightLow, _))
  | leftHigh >= rightLow = findBoth' p' (shrinkIntervalStep a) (shrinkIntervalStep b)
  | otherwise = findBoth p' a b

findBoth :: Poly Rational -> Algebraic -> Algebraic -> (Rational, Rational)
findBoth p' a@(Algebraic _ (leftLow, leftHigh)) b@(Algebraic _ (rightLow, rightHigh))
  | numLarge - numSmall == diff = (leftHigh, rightLow)
  | otherwise = findBoth p' a' b'
  where
    numSmall = numRootsInInterval p' (leftHigh, rightLow)
    numLarge = numRootsInInterval p' (leftLow, rightHigh)
    diff = length $ filter id [signAtAlgebraic a p' == Zero, signAtAlgebraic b p' == Zero]
    a' = shrinkIntervalStep a
    b' = shrinkIntervalStep b-}

---------------- The actual counting ----------------------

{-
findCritcalPointsPoly' :: Rational -> Poly Rational -> Rational -> [CriticalPoint]
findCritcalPointsPoly' _low p = go _low
  where
    p' = derP p
    go low high = case n of
      0 -> []
      1 -> [(Algebraic p' (low, high), criticalType low p' high)]
      _ -> findCritcalPointsPoly' low p mid ++ findCritcalPointsPoly' mid p high
      where
        n = numRootsInInterval p' (low, high)
        mid = (low + high) / 2-}

-- Assumes only one root in [low, high)
criticalType :: Rational -> Poly Rational -> Rational -> Critical
criticalType low p' high = criticalType' s1 s2
  where
    s1 = signAtRational low p'
    s2 = signRight low p' high

criticalType' :: Sign -> Sign -> Critical
criticalType' s1 s2 = case (s1, s2) of
  (Neg, Pos) -> Minimum
  (Pos, Neg) -> Maximum
  _          -> Saddle

-- Finds the sign of p to the right of the root in the interval [low, high)
signRight :: Rational -> Poly Rational -> Rational -> Sign
signRight low p high
  | n == 0 = signAtRational low p
  | otherwise = signRight mid p high
  where
    n = numRootsInInterval p (low, high)
    mid = (low + high) / 2

------------ Between Polynomials ------------------------

findCriticalPointSeparation :: Poly Rational -> Separation Rational -> Poly Rational -> Maybe CriticalPoint
findCriticalPointSeparation p1 s p2 = case criticalType' s1 s2 of
  Saddle -> Nothing
  c      -> Just (fromPWSeparation s, c)
  where
    p1' = derP p1
    p2' = derP p2
    (s1, s2) = case s of
      PW.Dyadic x    -> (signAtRational x p1', signAtRational x p2')
      PW.Algebraic x -> let
        x' = fromPWAlgebraic x
        in (signAtAlgebraic x' p1', signAtAlgebraic x' p2')

------------- At interval endpoints ---------------------

firstPoly :: [Either (Poly Rational) (Separation Rational)] -> Poly Rational
firstPoly []               = error "No poly found (should not happen)"
firstPoly ((Right _) : xs) = firstPoly xs
firstPoly (Left p : _)     = p

-- findIntervalPoint :: Rational -> [Either (Poly Rational) (Separation Rational)] ->  CriticalPoint
-- findIntervalPoint x pw
--   | isConstP p = (a, Saddle) -- This is a potential interpretation of this result
--   | otherwise = case compare p' zero of
--   LT -> (a, Minimum)
--   GT -> (a, Maximum)
--   EQ -> case compare p'' zero of
--     LT -> (a, Minimum)
--     GT -> (a, Maximum)
--     EQ -> error ("Should not happen " ++ show p)
--   where
--     p = firstPoly pw
--     p' = evalP (derP p) x
--     p'' = evalP (derP $ derP p) x
--     a = toAlgebraic x

----------- Piecewise polynomials -----------------------

-- Three types of critical points:
-- 1) 0 and 1
-- 2) Points separating pieces
-- 3) Points in the pieces
findCritcalPointsPW :: PiecewisePoly Rational -> [CriticalPoint]
findCritcalPointsPW pw = trace (showPW pw) midPoints
  where
    --zeroPoint = findIntervalPoint zero pwList
    --onePoint = findIntervalPoint one pwList
    midPoints = findMidPoints pwList
    pwList = linearizePW pw

findMidPoints :: [Either (Poly Rational) (Separation Rational)] -> [CriticalPoint]
findMidPoints [] = []
findMidPoints [_] = []
findMidPoints [_, _] = []
findMidPoints (Right s1 : Left p : Right s2 : xs) =
  findCritcalPointsPoly s1 p s2 ++ findMidPoints (Left p : Right s2 : xs)
findMidPoints (Left p1 : Right s : Left p2 : xs) = case findCriticalPointSeparation p1 s p2 of
  Nothing -> rest
  Just c  -> c : rest
  where
    rest = findMidPoints (Right s : Left p2 : xs)
findMidPoints xs = error ("Should not happen" ++ show xs)

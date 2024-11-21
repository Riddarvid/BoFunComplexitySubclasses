module Exploration.Critical (
  Critical(..),
  CriticalPoint,
  findCritcalPointsPW,
  criticalPointBetweenPieces,
  criticalPointsInPiece
) where
import           Algebraic          (AlgRep (AlgRep),
                                     Algebraic (Algebraic, Rational),
                                     fromPWSeparation, shrinkIntervalStep,
                                     signAtAlgebraic, signAtRational)
import           DSLsofMath.PSDS    (Poly, derP, isConstP)
import           Poly.PiecewisePoly (PiecewisePoly, linearizePW, showPW)
import           Poly.Utils         (numRootsInInterval)
import           Utils              (Sign (..))

data Critical = Maximum | Minimum | Saddle
  deriving (Show, Eq)

type CriticalPoint = (Algebraic, Critical)

----------- Piecewise polynomials -----------------------

-- Two types of critical points:
-- 1) Points between pieces
-- 2) Points in the pieces
-- One could also argue that the points at the end of the interval (0,1) are of interest,
-- but we have decided not to handle them here.
findCritcalPointsPW :: PiecewisePoly Rational -> [CriticalPoint]
findCritcalPointsPW pw = midPoints
  where
    midPoints = findMidPoints pwList
    pwList = map (fmap fromPWSeparation) $ linearizePW pw

findMidPoints :: [Either (Poly Rational) Algebraic] -> [CriticalPoint]
findMidPoints [] = []
findMidPoints [_] = []
findMidPoints [_, _] = []
findMidPoints (Right s1 : Left p : Right s2 : xs) =
  criticalPointsInPiece s1 p s2 ++ findMidPoints (Left p : Right s2 : xs)
findMidPoints (Left p1 : Right s : Left p2 : xs) = case criticalPointBetweenPieces p1 s p2 of
  Nothing -> rest
  Just c  -> c : rest
  where
    rest = findMidPoints (Right s : Left p2 : xs)
findMidPoints xs = error ("Should not happen" ++ show xs)

------------ Between Polynomials ------------------------

-- It also makes sense to simply return a Saddle in the case of a 0-derivative
criticalPointBetweenPieces :: Poly Rational -> Algebraic -> Poly Rational -> Maybe CriticalPoint
criticalPointBetweenPieces p1 s p2 = case criticalType' s1 s2 of
  Saddle -> Nothing
  c      -> Just (s, c)
  where
    p1' = derP p1
    p2' = derP p2
    (s1, s2) = (signAtAlgebraic s p1', signAtAlgebraic s p2')

------------ In polynomials ------------------------

criticalPointsInPiece :: Algebraic -> Poly Rational -> Algebraic -> [CriticalPoint]
criticalPointsInPiece a1 p a2
  | a1 >= a2 = error "a1 must be < a2"
  | isConstP p = [] -- In reality, every point in the domain is a critical point for a constant
                    -- function. However, we have chosen to return an empty list in this case,
                    -- as it is more practical to us.
criticalPointsInPiece a1 p a2 = criticalPointsInPiece' r1 p r2
  where
    (r1, r2) = findRationalEndpoints a1 p a2

------------------ Finding rational endpoints -------------------

-- This range is exclusive
findRationalEndpoints :: Algebraic -> Poly Rational -> Algebraic -> (Rational, Rational)
findRationalEndpoints s1 p = findBoth p' s1
  where
    p' = derP p

findBoth :: Poly Rational -> Algebraic -> Algebraic -> (Rational, Rational)
findBoth p' a b
  | overlap a b = findBoth p' (shrinkIntervalStep a) (shrinkIntervalStep b)
  | otherwise = findBoth' p' a b

findBoth' :: Poly Rational -> Algebraic -> Algebraic -> (Rational, Rational)
findBoth' p' a b
  -- | traceShow (numSmall, numLarge, diff) False = undefined
  | numLarge - numSmall == diff = small
  | otherwise = findBoth' p' a' b'
  where
    small = smallInterval a b
    numSmall = numRootsInInterval p' small
    numLarge = numRootsInInterval p' $ largeInterval a b
    diff = expectedDiff p' a + expectedDiff p' b
    a' = shrinkIntervalStep a
    b' = shrinkIntervalStep b

overlap :: Algebraic -> Algebraic -> Bool
overlap a b = algMax a >= algMin b

largeInterval :: Algebraic -> Algebraic -> (Rational, Rational)
largeInterval a b = (algMin a, algMax b)

smallInterval :: Algebraic -> Algebraic -> (Rational, Rational)
smallInterval a b = (algMax a, algMin b)

algMin :: Algebraic -> Rational
algMin (Rational r)                    = r
algMin (Algebraic (AlgRep _ (low, _))) = low

algMax :: Algebraic -> Rational
algMax (Rational r)                     = r
algMax (Algebraic (AlgRep _ (_, high))) = high

expectedDiff :: Poly Rational -> Algebraic -> Int
expectedDiff p' a = case a of
  Rational _  -> 0
  Algebraic _ -> if signAtAlgebraic a p' == Zero then 1 else 0

---------------- The actual counting ----------------------

criticalPointsInPiece' :: Rational -> Poly Rational -> Rational -> [CriticalPoint]
criticalPointsInPiece' _low p = go _low
  where
    p' = derP p
    go low high = case numRootsInInterval p' (low, high) of
      0 -> []
      1 -> [(Algebraic $ AlgRep p' (low, high), criticalType low p' high)]
      _ -> go low mid ++ go mid high
      where
        mid = (low + high) / 2


-------------- Utils ----------------------

-- Assumes only one root in (low, high)
criticalType :: Rational -> Poly Rational -> Rational -> Critical
criticalType low p' high = criticalType' sign1 sign2
  where
    sign1 = signLeft low p' high
    sign2 = signRight low p' high

criticalType' :: Sign -> Sign -> Critical
criticalType' s1 s2 = case (s1, s2) of
  (Neg, Pos) -> Minimum
  (Pos, Neg) -> Maximum
  _          -> Saddle

data ShrinkDir = SLeft | SRight

signLeft :: Rational -> Poly Rational -> Rational -> Sign
signLeft = signDirection SLeft

signRight :: Rational -> Poly Rational -> Rational -> Sign
signRight = signDirection SRight

-- Shrink to the right until we've passed the critical point
signDirection :: ShrinkDir -> Rational -> Poly Rational -> Rational -> Sign
signDirection dir low p' high
  | n == 0 = signAtRational mid p'
  | otherwise = case dir of
    SLeft  -> signLeft low p' mid
    SRight -> signRight mid p' high
  where
    n = numRootsInInterval p' (low, high)
    mid = (low + high) / 2

{-# LANGUAGE InstanceSigs #-}
module Exploration.Critical (
  Critical(..),
  Location(..),
  CriticalPoint,
  UncertainCriticalPoint(..),
  determineUncertain,
  critcalPointsPW,
  criticalPointBetweenPieces,
  criticalPointsInPiece
) where
import           Algebraic          (AlgRep (AlgRep),
                                     Algebraic (Algebraic, Rational),
                                     fromPWSeparation, shrinkIntervalStep,
                                     signAtAlgebraic, signAtRational)
import           Data.List          (sort)
import           DSLsofMath.PSDS    (Poly, constP, derP, evalP)
import           Poly.PiecewisePoly (PiecewisePoly, linearizePW, pieces)
import           Poly.Utils         (numRootsInInterval, removeDoubleRoots)
import           Utils              (Sign (..))

data Location = Point Algebraic | Range Algebraic Algebraic
  deriving (Show)

data Critical = Maximum | Minimum
  deriving (Show, Eq, Ord)

type CriticalPoint = (Location, Critical)

-- UncertainCriticalPoint can represent either a fixed point with a known critical type,
-- or a range with unknown critical type (if any). Ranges always represent constant
-- polynomial pieces, and therefore have a constant value, which is also captured in
-- the data type.
data UncertainCriticalPoint = UPoint Algebraic Critical | URange Algebraic Algebraic Rational
  deriving (Show)

instance Eq Location where
  (==) :: Location -> Location -> Bool
  Point x1 == Point x2                   = x1 == x2
  Range start1 end1 == Range start2 end2 = (start1, end1) == (start2, end2)
  Point x1 == Range start end            = x1 == start && x1 == end
  r@(Range _ _) == p@(Point _)           = p == r

instance Ord Location where
  (<=) :: Location -> Location -> Bool
  Point x1 <= Point x2             = x1 <= x2
  Range start1 _ <= Range start2 _ = start1 <= start2
  Point x <= Range start _         = x <= start
  Range _ end <= Point x           = end <= x

----------- Piecewise polynomials -----------------------

-- Two types of critical points:
-- 1) Points between pieces
-- 2) Points in the pieces
-- 3) The endpoints of [0, 1]
-- findMidPoints finds the critical points of type 1 & 2.
-- addEndPoints handles 3.
-- Lastly, determineUncertain takes care of identifying whether constant pieces
-- actually represent an infinite number of maxima/minima or not.
critcalPointsPW :: PiecewisePoly Rational -> [CriticalPoint]
critcalPointsPW pw = determineUncertain points
  where
    points = addEndPoints (pieces pw) midPoints
    midPoints = findMidPoints pwList
    pwList = map (fmap fromPWSeparation) $ linearizePW pw

------------- Conversion from UncertainCriticalPoints to CriticalPoints ----------
-- TODO-NEW explain this process

determineUncertain :: [UncertainCriticalPoint] -> [CriticalPoint]
determineUncertain [URange start end _] = [(Range start end, Maximum), (Range start end, Minimum)]
determineUncertain uPoints = sort (ranges ++ points)
  where
    ranges = handleRanges uPoints
    points = handlePoints uPoints

handlePoints :: [UncertainCriticalPoint] -> [CriticalPoint]
handlePoints [] = []
handlePoints (x : xs) = case x of
  URange {}  -> rest
  UPoint a t -> (Point a, t) : rest
  where
    rest = handlePoints xs

handleRanges :: [UncertainCriticalPoint] -> [CriticalPoint]
handleRanges xs = handleRanges' startSign xs
  where
    startSign = findStartCriticalType xs

findStartCriticalType :: [UncertainCriticalPoint] -> Critical
findStartCriticalType (UPoint _ t : _)                = t
findStartCriticalType (URange {} : UPoint _ t : _) = oppositeType t
findStartCriticalType (r@(URange _ _ v1) : URange _ _ v2 : xs) = case compare v1 v2 of
  LT -> Minimum
  GT -> Maximum
  EQ -> findStartCriticalType (r : xs)
findStartCriticalType _ = error "Should not happen"

handleRanges' :: Critical -> [UncertainCriticalPoint] -> [CriticalPoint]
handleRanges' _ [] = []
handleRanges' prev [URange start end _] = [(Range start end, oppositeType prev)]
handleRanges' _ (UPoint _ t : xs) = handleRanges' t xs
handleRanges' prev (URange start1 end1 v1 : URange start2 end2 v2 : xs) = case compare v1 v2 of
  EQ -> handleRanges' prev (URange start1 end2 v1 : xs)
  LT -> case prev of
    Maximum -> (Range start1 end1, Minimum) : handleRanges' Minimum rest
    Minimum -> handleRanges' Minimum rest
  GT -> case prev of
    Minimum -> (Range start1 end1, Maximum) : handleRanges' Maximum rest
    Maximum -> handleRanges' Maximum rest
  where
    rest = URange start2 end2 v2 : xs
handleRanges' prev (URange start end _ : p@(UPoint _ t) : xs) = case (prev, t) of
  (Maximum, Maximum) -> (Range start end, Minimum) : rest
  (Minimum, Minimum) -> (Range start end, Maximum) : rest
  _                  -> rest
  where
    rest = handleRanges' prev (p : xs)

---------- Identifying critical points at the endpoints of [0, 1] ----------------

addEndPoints :: [Poly Rational] -> [UncertainCriticalPoint] -> [UncertainCriticalPoint]
addEndPoints ps [] = case compare zeroVal oneVal of
  LT -> [UPoint (Rational 0) Minimum, UPoint (Rational 1) Maximum]
  GT -> [UPoint (Rational 0) Maximum, UPoint (Rational 1) Minimum]
  EQ -> [URange (Rational 0) (Rational 1) zeroVal]
  where
    zeroVal = evalP (head ps) 0
    oneVal = evalP (last ps) 1
addEndPoints _ xs = addFirst $ addLast xs

addFirst :: [UncertainCriticalPoint] -> [UncertainCriticalPoint]
addFirst [] = error "Should not happen"
addFirst rest@(x : _) = case x of
  URange {}        -> rest
  UPoint _ Maximum -> UPoint endPoint Minimum : rest
  UPoint _ Minimum -> UPoint endPoint Minimum : rest
  where
    endPoint = Rational 0

addLast :: [UncertainCriticalPoint] -> [UncertainCriticalPoint]
addLast [] = error "Should not happen"
addLast xs = case last xs of
  URange {}        -> xs
  UPoint _ Maximum -> xs ++ [UPoint endPoint Minimum]
  UPoint _ Minimum -> xs ++ [UPoint endPoint Maximum]
  where
    endPoint = Rational 1

---------- Identifying critical points in or between pieces -------------
-- TODO-NEW explain

findMidPoints :: [Either (Poly Rational) Algebraic] -> [UncertainCriticalPoint]
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

criticalPointBetweenPieces :: Poly Rational -> Algebraic -> Poly Rational -> Maybe UncertainCriticalPoint
criticalPointBetweenPieces p1 s p2 = do
  c <- criticalType' s1 s2
  return (UPoint s c)
  where
    p1' = derP p1
    p2' = derP p2
    (s1, s2) = (signAtAlgebraic s p1', signAtAlgebraic s p2')

------------ In polynomials ------------------------

criticalPointsInPiece :: Algebraic -> Poly Rational -> Algebraic -> [UncertainCriticalPoint]
criticalPointsInPiece a1 _ a2
  | a1 >= a2 = error "a1 must be < a2"
criticalPointsInPiece a1 p a2 = case constP p of
  Just v  -> [URange a1 a2 v]
  Nothing -> criticalPointsInPiece' r1 p r2
  where
    (r1, r2) = findRationalEndpoints a1 p a2

criticalPointsInPiece' :: Rational -> Poly Rational -> Rational -> [UncertainCriticalPoint]
criticalPointsInPiece' _low p = go _low
  where
    p' = derP p
    noDoubleRootsP' = removeDoubleRoots p'
    go low high = case numRootsInInterval noDoubleRootsP' (low, high) of
      0 -> []
      1 -> case criticalType low p' high of
        Nothing -> []
        Just t  -> [UPoint (Algebraic $ AlgRep p' (low, high)) t]
      _ -> go low mid ++ go mid high
      where
        mid = (low + high) / 2

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

-------------- Utils ----------------------

oppositeType :: Critical -> Critical
oppositeType Maximum = Minimum
oppositeType Minimum = Maximum

-- Assumes only one root in (low, high)
criticalType :: Rational -> Poly Rational -> Rational -> Maybe Critical
criticalType low p' high = criticalType' sign1 sign2
  where
    sign1 = signLeft low p' high
    sign2 = signRight low p' high

criticalType' :: Sign -> Sign -> Maybe Critical
criticalType' s1 s2 = case (s1, s2) of
  (Neg, Pos) -> Just Minimum
  (Pos, Neg) -> Just Maximum
  _          -> Nothing

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

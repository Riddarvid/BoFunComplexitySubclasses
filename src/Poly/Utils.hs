module Poly.Utils (
  findDegreeBothPW,
  findDegreePW,
  countPieces,
  minDegree,
  numRootsInInterval,
  removeDoubleRoots,
  isRoot
) where
import           DSLsofMath.Algebra   (AddGroup, Additive (zero, (+)), MulGroup,
                                       Multiplicative, product, (-))
import           DSLsofMath.PSDS      (Poly, degree, evalP, isZero, yun)
import           Poly.PiecewisePoly   (BothPW (BothPW), PiecewisePoly,
                                       Separation, linearizePW, pieces)
import           Poly.PolyCmp         (numRoots)
import           Poly.PolynomialExtra (scaleInput, translateInput)
import           Prelude              hiding (product, (+), (-))

findDegreeBothPW :: (Eq a, AddGroup a, MulGroup a) => BothPW a -> Int
findDegreeBothPW (BothPW pw _) = findDegreePW pw

findDegreePW :: (Eq a, AddGroup a, MulGroup a) => PiecewisePoly a -> Int
findDegreePW pw = maximum $ map degree $ extractPolys $ linearizePW pw

extractPolys :: [Either (Poly a) (Separation b)] -> [Poly a]
extractPolys []               = []
extractPolys (Left poly : xs) = poly : extractPolys xs
extractPolys (_ : xs)         = extractPolys xs

minDegree :: (Eq a, AddGroup a, MulGroup a) => [PiecewisePoly a] -> Int
minDegree xs = minimum $ map findDegreePW xs

-- Counts the number of pieces of a piecewise polynomial.
-- linearizePW results in a list of either pieces or separations,
-- thus we can count the number of pieces by filtering on isLeft.
countPieces :: (AddGroup a, MulGroup a, Eq a) => PiecewisePoly a -> Int
countPieces = length . pieces

-- Transforms the polynomial p(x) into p((x - a) / diff) in order to be able
-- to use numRoots. Counts in the interval (low, high).
numRootsInInterval :: ( AddGroup a, MulGroup a, Ord a, Show a) => Poly a -> (a, a) -> Int
numRootsInInterval p _
  | isZero p = error "P is constant 0 and therefore has an infinite number of roots"
numRootsInInterval p (low, high) = numRoots p'
  where
    diff = high - low
    p' = removeDoubleRoots $ scaleInput diff $ translateInput low p

-- Creates a polynomial with only single roots. The new polynomial has roots in exactly the points
-- where the input polynomial has roots. No other guarantees are given for the new polynomial.
removeDoubleRoots :: (Eq a, MulGroup a, AddGroup a) => Poly a -> Poly a
removeDoubleRoots p = product polys
  where
    polys = yun p

isRoot :: (Eq a, AddGroup a, Multiplicative a) =>a -> Poly a -> Bool
isRoot x p = evalP p x == zero

module Poly.Utils (
  findDegreeBothPW,
  findDegreePW,
  countPieces,
  minDegree,
  numRootsInInterval,
  removeDoubleRoots
) where
import           Data.Either        (isLeft)
import           DSLsofMath.Algebra (AddGroup, Additive (zero), MulGroup,
                                     product, (-))
import           DSLsofMath.PSDS    (Poly (P), degree, evalP, yun)
import           Poly.PiecewisePoly (BothPW (BothPW), PiecewisePoly, Separation,
                                     linearizePW)
import           Poly.PolyCmp       (numRoots)
import           Prelude            hiding (product, (+), (-))

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
countPieces pw = length $ filter isLeft $ linearizePW pw

-- Transforms the polynomial p(x) into p((x - a) / diff) in order to be able
-- to use numRoots.
-- Inclusive.
-- TODO use functions from PSDS.
numRootsInInterval :: ( AddGroup a, MulGroup a, Ord a, Show a) => Poly a -> (a, a) -> Int
numRootsInInterval p (low, high)
  -- If the interval is a single point, then we can't scale it up to the range (0,1).
  -- Instead, we simply check the value of the polynomial in that point, and if
  -- the result is zero, then by definition it has a root there, otherwise it has no roots.
  -- | traceShow p'' False = undefined
  | diff == zero = if evalP p low == zero then 1 else 0
  | otherwise = numRoots p''
  where
    diff = high - low
    p' = fmap (\c -> P [c]) p
    p'' = evalP p' (P [low, diff])

-- TODO QuickCheck
-- Creates a polynomial with only single roots. The new polynomial has roots in exactly the points
-- where the input polynomial has roots. No other guarantees are given for the new polynomial.
removeDoubleRoots :: (Eq a, MulGroup a, AddGroup a) => Poly a -> Poly a
removeDoubleRoots p = product polys
  where
    polys = yun p

module Poly.Utils (
  countMaxima,
  findDegreeBothPW,
  findDegreePW,
  findDegreePoly,
  countPieces,
  minDegree
) where
import           Data.Complex       (Complex ((:+)), realPart)
import           Data.Either        (isLeft)
import           Data.Foldable      (find)
import           Data.Maybe         (fromJust)
import           DSLsofMath.Algebra (AddGroup, Additive (zero), MulGroup,
                                     Multiplicative)
import           DSLsofMath.PSDS    (Poly (P), derP, evalP)
import           Poly.PiecewisePoly (BothPW (BothPW), PiecewisePoly, Separation,
                                     Separation' (..), linearizePW)
import           Polynomial.Roots   (roots)

countMaxima :: (Real a) => PiecewisePoly a -> Int
countMaxima pw = countMaxima' $ linearizePW (fmap realToFrac pw :: PiecewisePoly Double)

countMaxima' :: (RealFloat a, AddGroup a, Multiplicative a) => [Either (Poly a) (Separation a)] -> Int
countMaxima' (Right _ : xs) = countMaxima' xs
countMaxima' (Left p1 : Right s : Left p2 : xs)
  | hasMaximum p1 p2 s = 1 + rest
  | otherwise = rest
  where
    rest = countMaxima' (Left p2 : xs)
countMaxima' _ = 0

-- The problem right now is finding the correct root.
-- Another problem is that the root should be a real number but currently
-- we're casting it to a rational number, which of course changes the value.
-- Really, we should make the function work with real numbers instead.
hasMaximum :: (RealFloat a, AddGroup a, Multiplicative a) => Poly a -> Poly a -> Separation a -> Bool
hasMaximum p1 p2 s = evalP p1' (p - 0.001) > zero && evalP p2' (p + 0.001) < zero
  where
    p1' = derP p1
    p2' = derP p2
    p = case s of
      Dyadic p'                        -> p'
      Algebraic (P pPoly, (low, high)) -> root
        where
          pRoots = map realPart $ roots 1e-16 1000 $ map (:+ zero) pPoly
          root = fromJust $ find (\n -> low < n && n < high) pRoots

-- Degree finding

findDegreeBothPW :: (Eq a, AddGroup a, MulGroup a) => BothPW a -> Int
findDegreeBothPW (BothPW pw _) = findDegreePW pw

findDegreePW :: (Eq a, AddGroup a, MulGroup a) => PiecewisePoly a -> Int
findDegreePW pw = maximum $ map findDegreePoly $ extractPolys $ linearizePW pw

findDegreePoly :: (Eq a, Additive a) => Poly a -> Int
findDegreePoly (P xs) = length $ dropZeroes xs

dropZeroes :: (Eq a, Additive a) => [a] -> [a]
dropZeroes [] = []
dropZeroes (n : ns)
  | n == zero = case dropZeroes ns of
    []  -> []
    ns' -> n : ns'
  | otherwise = n : dropZeroes ns

extractPolys :: [Either (Poly a) (Separation b)] -> [Poly a]
extractPolys []               = []
extractPolys (Left poly : xs) = poly : extractPolys xs
extractPolys (_ : xs)         = extractPolys xs

minDegree :: (Eq a, AddGroup a, MulGroup a) => [PiecewisePoly a] -> Int
minDegree xs = minimum $ map findDegreePW xs

-- Piece counting

-- Counts the number of pieces of a piecewise polynomial.
-- linearizePW results in a list of either pieces or separations,
-- thus we can count the number of pieces by filtering on isLeft.
countPieces :: (AddGroup a, MulGroup a, Eq a) => PiecewisePoly a -> Int
countPieces pw = length $ filter isLeft $ linearizePW pw

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ComputeAll (
  genAllBoths
) where
import           Algorithm.Algor          (DecTree)
import           Algorithm.GenAlg         (genAlgThinMemo)
import           Algorithm.GenAlgPW       (computeMin)
import           BDD                      (BDDFun, bddAsc, pick)
import           BDD.BDDInstances         ()
import           Data.Complex             (Complex ((:+)), realPart)
import           Data.DecisionDiagram.BDD (AscOrder, ItemOrder, false, notB,
                                           true)
import           Data.Either              (isLeft)
import           Data.Foldable            (find)
import qualified Data.IntSet              as IS
import           Data.Maybe               (fromJust, isJust)
import           Data.Set                 (Set, fromList, size, toList)
import           Debug.Trace              (trace, traceShow)
import           DSLsofMath.Algebra       (AddGroup, Additive (zero), MulGroup,
                                           Multiplicative (one))
import           DSLsofMath.PSDS          (Poly (P), derP, evalP)
import           Numeric                  (fromRat)
import           PiecewisePoly            (PiecewisePoly, Separation,
                                           Separation' (..), linearizePW,
                                           minPWs, piecewiseFromPoly, printPW,
                                           printPWAny, showPW)
-- import           Polynomial.Roots         (roots)

test :: IO ()
test = mapM_ print (genAllPPs 3)

-- Counts the number of pieces of a piecewise polynomial.
-- linearizePW results in a list of either pieces or separations,
-- thus we can count the number of pieces by filtering on isLeft.
countPieces :: (AddGroup a, MulGroup a, Eq a) => PiecewisePoly a -> Int
countPieces pw = length $ filter isLeft $ linearizePW pw

predPW :: (AddGroup a, MulGroup a, Eq a) => Int -> PiecewisePoly a -> Bool
predPW n pw = countPieces pw == n

--uniquePPs :: [([Bool], Set (Poly Rational))] -> Set (Set (Poly Rational))
--uniquePPs xs = fromList $ map snd xs

filterPieces :: Int -> [([Bool], PiecewisePoly Rational)] -> [([Bool], PiecewisePoly Rational)]
filterPieces pieces = filter (\(_, pw) -> predPW pieces pw)

genAllPPs :: Int -> [([Bool], PiecewisePoly Rational)]
genAllPPs n = map (\out -> (out, computeMin (bddFromOutput n out :: BDDFun AscOrder))) $
  outputPermutations n

boolToBDD :: Bool -> BDDFun a
boolToBDD True  = true
boolToBDD False = false

bddFromOutput :: ItemOrder a => Int -> [Bool] -> BDDFun a
bddFromOutput bits = bddFromOutput' bits 0

bddFromOutput' :: ItemOrder a => Int -> Int -> [Bool] -> BDDFun a
bddFromOutput' 0 varN out = boolToBDD (out !! varN)
bddFromOutput' bits varN out = pick bits
  (bddFromOutput' (bits - 1) (2 * varN + 1) out)
  (bddFromOutput' (bits - 1) (2 * varN) out)

outputPermutations :: Int -> [[Bool]]
outputPermutations n = permutations (2^n)

permutations :: Int -> [[Bool]]
permutations 0 = [[]]
permutations n = do
  v <- [False, True]
  vs <- permutations (n - 1)
  return (v : vs)

-- Utils

-- Takes a set of Polynomials and creates the piecewise polynomial that is the minimum
-- of the polynomials at each point.
polySetToPW :: (Show a, AddGroup a, MulGroup a, Ord a) => Set (Poly a) -> PiecewisePoly a
polySetToPW polys = minPWs $ map piecewiseFromPoly $ toList polys

test2 :: IO ()
test2 = mapM_ printPWAny $
  filter (\p -> countMaxima p == 2) polys
  where
    polys :: [PiecewisePoly Double]
    polys = map (fmap fromRational . snd) (genAllPPs 4)

countMaxima :: (AddGroup a, Multiplicative a, MulGroup a, Real a) => PiecewisePoly a -> Int
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

roots = undefined

test3 :: BothPW
test3 = piecewiseBoth $ bddAsc $ bddFromOutput 2 [False, True, False, False]

findSimplest :: [BothPW]
findSimplest = map snd $ filter (\(degree, _) -> degree == minDegree) $
  zip degrees allWith2maxima
  where
    minDegree = minimum degrees
    degrees = map findDegree allWith2maxima
    allWith2maxima = filter (\(BothPW pw _) -> countMaxima pw == 2) allBoths
    allBoths = genAllBoths 4

findDegree :: BothPW -> Int
findDegree (BothPW pw _) = findDegree' pw

findDegree' :: PiecewisePoly Rational -> Int
findDegree' pw = maximum $ map findDegree'' $ extractPolys $ linearizePW pw

findDegree'' :: Poly Rational -> Int
findDegree'' (P xs) = length $ dropZeroes xs

dropZeroes :: [Rational] -> [Rational]
dropZeroes [] = []
dropZeroes (n : ns)
  | n == zero = case dropZeroes ns of
    []  -> []
    ns' -> n : ns'
  | otherwise = n : dropZeroes ns

extractPolys :: [Either (Poly Rational) (Separation Rational)] -> [Poly Rational]
extractPolys []               = []
extractPolys (Left poly : xs) = poly : extractPolys xs
extractPolys (_ : xs)         = extractPolys xs

data BothPW = BothPW (PiecewisePoly Rational) [(Poly Rational, DecTree)]

instance Show BothPW where
  show :: BothPW -> String
  show (BothPW pw lookupPolys) = showPW pw ++ "\n" ++ unlines
    (map (\(poly, al) -> show poly ++ ":\t\t" ++ show al) lookupPolys)

-- Computes the PWs via genAlgBoth but then converts the resulting set of polynomials
-- to a PW, and gives a lookup table from poly to decision tree.
piecewiseBoth :: ItemOrder o => BDDFun o -> BothPW
piecewiseBoth f = BothPW pw lookupTable
  where
    boths :: [(Poly Rational, DecTree)]
    boths = toList $ genAlgThinMemo f

    lookupTable = map (\(poly, al) -> (poly, al)) boths

    pw = minPWs $ map (\(poly, _) -> piecewiseFromPoly poly) boths

genAllBoths :: Int -> [BothPW]
genAllBoths n = map (\out -> piecewiseBoth (bddFromOutput n out :: BDDFun AscOrder)) $
  outputPermutations n

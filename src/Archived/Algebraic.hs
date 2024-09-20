-- This module contains experimental code for writing instances of
-- Additive and Multiplicative for algebraic numbers.
-- The methodology used is based on
-- https://math.stackexchange.com/questions/2911256/a-way-to-represent-algebraic-numbers-in-a-computer
-- Right now the implementation quickly becomes incredibly slow. Specifically, the function
-- myUDecomp. As a result, we have decided to explore other solutions instead.

module Archived.Algebraic () where

{-
module Archived.Algebraic (
  sumOfRationalsProp,
  sum2Prop,
  productOfRationalsProp,
  addCommProp,
  mulCommProp,
  negateProp,
  assocProp
) where
import           Algebraic             (Algebraic (Algebraic),
                                        shrinkIntervalStep)
import           Archived.MatrixBridge (Matrix (nrows), elementwise, flatten,
                                        fromLists, identity, toLists)
import           Data.Foldable         (find)
import           Data.Maybe            (fromJust)
import           Debug.Trace           (trace, traceShow)
import           DSLsofMath.Algebra    (AddGroup (..), Additive (..),
                                        MulGroup ((/)), Multiplicative (..),
                                        sum, (-))
import           DSLsofMath.PSDS       (Poly (P), evalP, normalPoly, toMonic,
                                        xP)
import           Poly.Utils            (numRootsInInterval, removeDoubleRoots)
import           Prelude               hiding (negate, product, sum, (*), (+),
                                        (-), (/))
import           Test.QuickCheck       (Property, within, (===))

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (x' - 1, x' + 1) which is guaranteed to contain x'
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = Algebraic (P [negate x', one]) (x' - one, x' + one)
  where
    x' = toRational x

instance Additive Algebraic where
  zero :: Algebraic
  zero = toAlgebraic (zero :: Int)
  (+) :: Algebraic -> Algebraic -> Algebraic
  (+) = addAlgebraic

instance AddGroup Algebraic where
  negate :: Algebraic -> Algebraic
  negate (Algebraic (P coeffs) (low, high)) = Algebraic (P $ flipOdd coeffs) (negate high, negate low)

flipOdd :: AddGroup a => [a] -> [a]
flipOdd = flipOdd' False

flipOdd' :: AddGroup a => Bool -> [a] -> [a]
flipOdd' _ []           = []
flipOdd' False (x : xs) = x : flipOdd' True xs
flipOdd' True (x : xs)  = negate x : flipOdd' False xs

instance Multiplicative Algebraic where
  one :: Algebraic
  one = toAlgebraic (one :: Int)
  (*) :: Algebraic -> Algebraic -> Algebraic
  (*) = mulAlgebraic

instance Eq Algebraic where
  (==) :: Algebraic -> Algebraic -> Bool
  a == b = compare a b == EQ

instance Ord Algebraic where
  compare :: Algebraic -> Algebraic -> Ordering
  compare a b
    | trace ("comp\n" ++ show a ++ "\n" ++ show b) $ low > zero = GT
    | trace "second" $ high < zero = LT
    | evalP c zero == zero = EQ
    | otherwise = case numRootsInInterval c (zero, high) of
      0 -> LT
      1 -> GT
      _ -> error "Should only be able to have 0 or 1 roots"
    where
      Algebraic c (low, high) = a - b

mulAlgebraic :: Algebraic -> Algebraic -> Algebraic
mulAlgebraic a@(Algebraic pa _) b@(Algebraic pb _) = Algebraic ab' intAB
  where
    aMatrix = toCharacteristicMatrix $ toMonic pa
    bMatrix = toCharacteristicMatrix $ toMonic pb
    abMatrix = outerProduct aMatrix bMatrix
    ab = toCharacteristicPoly abMatrix
    ab' = removeDoubleRoots ab
    intAB = intervalMul a b ab'

addAlgebraic :: Algebraic -> Algebraic -> Algebraic
addAlgebraic a@(Algebraic pa _) b@(Algebraic pb _)
  | trace ("add\n" ++ show a ++ "\n" ++ show b) False = undefined
  -- | pb == zero = undefined -- Sanity check
  | otherwise = Algebraic aPlusB' intAB
  where
    (pa', pb') = rootPad pa pb
    aM = toCharacteristicMatrix $ toMonic pa'
    bM = toCharacteristicMatrix $ toMonic pb'
    n = nrows aM
    aiM = outerProduct aM (identity n)
    ibM = outerProduct (identity n) bM
    aPlusBM = aiM ^+^ ibM
    aPlusB = toCharacteristicPoly aPlusBM
    aPlusB' = removeDoubleRoots aPlusB
    intAB = intervalAdd a b aPlusB'

rootPad :: (Eq a, Additive a) => Poly a -> Poly a -> (Poly a, Poly a)
rootPad pa pb = {-traceShow (pa, pb) $ traceShowId $-} case compare lenA lenB of
  EQ -> (pa', pb')
  LT -> (P (replicate (lenB - lenA) zero ++ coeffsA), pb')
  GT -> (pa', P (replicate (lenA - lenB) zero ++ coeffsB))
  where
    pa'@(P coeffsA) = normalPoly pa
    pb'@(P coeffsB) = normalPoly pb
    lenA = length coeffsA
    lenB = length coeffsB

-- Uses the companion matrix
-- The input polynomial must be monic
toCharacteristicMatrix :: (AddGroup a, MulGroup a) => Poly a -> Matrix a
toCharacteristicMatrix (P coeffs) = fromLists $ zipWith (createRow (n - 1)) [-1 ..] $ init coeffs
  where
    n = length coeffs

createRow :: (AddGroup a, Multiplicative a) => Int -> Int -> a -> [a]
createRow n (-1) c = replicate (n - 1) zero ++ [negate c]
createRow n row c  =
  replicate row zero ++
  [one] ++
  replicate (n - row - 2) zero ++
  [negate c]

outerProduct :: Multiplicative a => Matrix a -> Matrix a -> Matrix a
outerProduct a b = flatten $ fmap (scaleMatrix b) a

scaleMatrix :: Multiplicative a => Matrix a -> a -> Matrix a
scaleMatrix m s = fmap (* s) m

toCharacteristicPoly :: (Show a, Ord a, AddGroup a, MulGroup a) => Matrix a -> Poly a
toCharacteristicPoly m = myUDet $ MyMatrix $ toLists mSubLambdaI
  where
    n = nrows m
    m' = fmap (\c -> P [c]) m
    mSubLambdaI = m' ^-^ scaleMatrix (identity n) xP

(^-^) :: (AddGroup a) => Matrix a -> Matrix a -> Matrix a
a ^-^ b = elementwise (-) a b

(^+^) :: (AddGroup a) => Matrix a -> Matrix a -> Matrix a
a ^+^ b = elementwise (+) a b

newtype MyMatrix a = MyMatrix [[a]]

instance (Show a) => Show (MyMatrix a) where
  show :: MyMatrix a -> String
  show (MyMatrix rows) = unlines $ map show rows

myUDet :: (MulGroup a, AddGroup a, Eq a, Show a) => MyMatrix a -> a
myUDet m = case myUDecomp m of
  Nothing        -> zero
  Just (u, f, s) -> divideList (s * myDiagProd u) f

divideList :: MulGroup a => a -> [(a, Int)] -> a
divideList n []            = n
divideList n ((_, 0) : fs) = divideList n fs
divideList n ((f, p) : fs) = divideList (n / f) ((f, p - 1) : fs)

myDiagProd :: Multiplicative a => MyMatrix a -> a
myDiagProd (MyMatrix rows) = go 0
  where
    n = length rows
    go i
      | i >= n = one
      | otherwise = (rows !! i !! i) * go (i + 1)


myUDecomp :: (Multiplicative a, AddGroup a, Eq a, Show a) => MyMatrix a -> Maybe (MyMatrix a, [(a, Int)], a)
myUDecomp m@(MyMatrix rows) = myUDecomp' m 0 (length rows) [] one

-- TODO add comments describing the steps
-- as well as the purpose of moving multiplications to a separate variable
myUDecomp' :: (AddGroup a, Eq a, Multiplicative a, Show a) => MyMatrix a -> Int -> Int -> [(a, Int)] -> a -> Maybe (MyMatrix a, [(a, Int)], a)
myUDecomp' u@(MyMatrix rows) i ncols f s
  | traceShow (i, ncols) False = undefined
  | i >= ncols = Just (u, f, s)
  | otherwise = do
      pivotIndex <- fmap fst $ find (\(_, row) -> row !! i /= zero) $ drop i $ zip [0 ..] rows
      let s' = if pivotIndex == i then s else negate s
      let u'@(MyMatrix rows') = swapRows u i pivotIndex
      let pivotElement = (rows' !! i) !! i -- Could be done in an earlier step
      let f' = (pivotElement, ncols - i - 1) : f -- This step seems to be what's slowing down the entire computation. It involves raising polynomials to potentially very large powers.
      let u'' = eliminiateSubRows u' i pivotElement
      myUDecomp' u'' (i + 1) ncols f' s'

eliminiateSubRows :: (AddGroup a, Multiplicative a) => MyMatrix a -> Int -> a -> MyMatrix a
eliminiateSubRows (MyMatrix rows) i pivotElement = MyMatrix (upper ++ lower'')
  where
    pivotRow = rows !! i
    (upper, lower) = splitAt (i + 1) rows
    lower' = map (map (* pivotElement)) lower
    lower'' = zipWith
      (\origRow mulRow -> let c = origRow !! i in subRows mulRow (scaleRow c pivotRow)) lower lower'

subRows :: AddGroup a => [a] -> [a] -> [a]
subRows = zipWith (-)

scaleRow :: Multiplicative a => a -> [a] -> [a]
scaleRow s = map (*s)

swapRows :: MyMatrix a -> Int -> Int -> MyMatrix a
swapRows m@(MyMatrix rows) i j = setRow (setRow m i rowJ) j rowI
  where
    rowI = rows !! i
    rowJ = rows !! j

setRow :: MyMatrix a -> Int -> [a] -> MyMatrix a
setRow (MyMatrix rows) i row = case end of
  []        -> error "Index out of bounds"
  (_: end') -> MyMatrix (start ++ row : end')
  where
    (start, end) = splitAt i rows

-- Calculate the new interval after having performed a multiplication.
-- This is done by first calculating a bounding interval which is guaranteed
-- to contain the desired root.
-- We then check whather the interval contains exactly one root.
-- If it does, we are done. Otherwise, we shrink the factors' intervals and try again.
intervalMul :: Algebraic -> Algebraic -> Poly Rational -> (Rational, Rational)
intervalMul a b = intervalMul' (sameSignInt a) (sameSignInt b)

sameSignInt :: Algebraic -> Algebraic
sameSignInt x@(Algebraic p (low, high))
  | low <= 0 && high <= 0 = x
  | low >= 0 && high >= 0 = x
  -- Here we assume low < 0 && high > 0
  | otherwise = case compare x zero of
    EQ -> zero
    LT -> Algebraic p (low, 0)
    GT -> Algebraic p (0, high)

-- Assumes that low and high in the intervals have the same sign.
intervalMul' :: Algebraic -> Algebraic -> Poly Rational -> (Rational, Rational)
intervalMul' a@(Algebraic _ intA) b@(Algebraic _ intB) pab =
  case numRootsInInterval pab intAB of
    0 -> error "Should have at least one root"
    1 -> intAB
    _ -> intervalMul' (shrinkIntervalStep a) (shrinkIntervalStep b) pab
  where
    intAB = boundInt intA intB

intervalAdd :: Algebraic -> Algebraic -> Poly Rational -> (Rational, Rational)
intervalAdd a@(Algebraic _ (lowA, highA)) b@(Algebraic _ (lowB, highB)) pab
  | traceShow (pab, intAPlusB, a, b) False = undefined
  | otherwise =
  case numRootsInInterval pab intAPlusB of
    0 -> error "Should have at least one root"
    1 -> intAPlusB
    _ -> intervalAdd (shrinkIntervalStep a) (shrinkIntervalStep b) pab
  where
    intAPlusB = (lowA + lowB, highA + highB)

boundInt :: (Multiplicative a, Additive a, Ord a) => (a, a) -> (a, a) -> (a, a)
boundInt intA@(lowA, highA) intB@(lowB, highB) = case sa of
  LEQ -> case sb of
    LEQ -> (highA * highB, lowA * lowB)
    GEQ -> (lowA * highB, lowB * highA)
  GEQ -> case sb of
    LEQ -> boundInt intB intA
    GEQ -> (lowA * lowB, highA * highB)
  where
    sa = fromJust $ intSign intA
    sb = fromJust $ intSign intB

data IntSign = LEQ | GEQ
  deriving (Eq)

intSign :: (Additive a, Ord a) => (a, a) -> Maybe IntSign
intSign (low, high)
  | low >= zero = Just GEQ
  | high <= zero = Just LEQ
  | otherwise = Nothing

sumOfRationalsProp :: Rational -> Rational -> Property
sumOfRationalsProp a b = toAlgebraic a + toAlgebraic b === toAlgebraic (a + b)

sum2Prop :: [Rational] -> Property
sum2Prop xs = sum (map toAlgebraic xs) === toAlgebraic (sum xs)

productOfRationalsProp :: Rational -> Rational -> Property
productOfRationalsProp a b = toAlgebraic a * toAlgebraic b === toAlgebraic (a * b)

addCommProp :: Algebraic -> Algebraic -> Property
addCommProp a b = within 5000000 $ a + b === b + a

mulCommProp :: Algebraic -> Algebraic -> Property
mulCommProp a b = a * b === b * a

negateProp :: Algebraic -> Property
negateProp a = a - a === zero

assocProp :: Algebraic -> Algebraic -> Algebraic -> Property
assocProp a b c = a + (b + c) === (a + b) + c
-}

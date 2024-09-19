{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant guard" #-}
module Algebraic (
  Algebraic(Algebraic),
  fromPWAlgebraic,
  toAlgebraic,
  toCharacteristicPoly,
  testMtoPoly,
  MyMatrix(MyMatrix)
) where
import           Control.Monad             (replicateM)
import           Data.Foldable             (find)
import           Data.List                 (nub, sort)
import           Data.Maybe                (fromJust)
import           Data.Ratio                ((%))
import           Debug.Trace               (trace, traceShow, traceShowId)
import           DSLsofMath.Algebra        (AddGroup (..), Additive (..),
                                            MulGroup ((/)), Multiplicative (..),
                                            product, sum, (-), (^+))
import           DSLsofMath.PSDS           (Poly (P), evalP, normalPoly,
                                            toMonic, xP, yun)
import           MatrixBridge              (Matrix (ncols, nrows), elementwise,
                                            flatten, fromLists, identity,
                                            toLists)
import           Poly.PolyCmp              (numRootsInclusive)
import           Prelude                   hiding (negate, product, sum, (*),
                                            (+), (-), (/))
import           Test.QuickCheck           (Arbitrary (arbitrary), Gen,
                                            Property, choose, chooseInt, within,
                                            (===))
import           Test.QuickCheck.Arbitrary (Arbitrary (shrink))

data Algebraic = Algebraic (Poly Rational) (Rational, Rational)
  deriving (Show)

instance Arbitrary Algebraic where
  arbitrary :: Gen Algebraic
  arbitrary = do
    n <- chooseInt (1, 2)
    (polys, roots) <- unzip <$> replicateM n genRootPoly
    let poly = removeDoubleRoots $ product polys
    int <- chooseInterval $ nub $ sort roots
    return $ Algebraic poly int

-- roots must be sorted and nubbed
chooseInterval :: [Rational] -> Gen (Rational, Rational)
chooseInterval roots = do
  i <- chooseInt (0, length roots - 1)
  let root = roots !! i
  let leftRoot = if i == 0 then root - 100 else roots !! (i - 1)
  let rightRoot = if i == length roots - 1 then root + 100 else roots !! (i + 1)
  let low = leftRoot + (root - leftRoot) * (1 % 10)
  let high = rightRoot - (rightRoot - root) * (1 % 10)
  return (low, high)

genRootPoly :: (Arbitrary a, AddGroup a, Multiplicative a) => Gen (Poly a, a)
genRootPoly = do
  n <- arbitrary
  return (P [negate n, one], n)


fromPWAlgebraic :: (Real a) => (Poly a, (a, a)) -> Algebraic
fromPWAlgebraic (p, (low, high)) = Algebraic (fmap toRational p) (toRational low, toRational high)

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (-x, x) which is guaranteed to contain x'
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = Algebraic (P [negate x', one]) (x', x')
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

-- TODO add QuickCheck properties to test the implementation

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

rootPad :: (Eq a, Additive a, Show a) => Poly a -> Poly a -> (Poly a, Poly a)
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

------------ Double root removal ---------------------------

-- TODO QuickCheck
removeDoubleRoots :: (Eq a, MulGroup a, AddGroup a) => Poly a -> Poly a
removeDoubleRoots p = product polys
  where
    polys = yun p

------------ Interval shrinking ----------------------------

-- Shrink the interval by dividing it into two pieces and checking which
-- one has a root.
shrinkInterval :: Algebraic -> Algebraic
shrinkInterval (Algebraic p (low, high))
  | nRoots > 1 || nRoots < 0 = error "Interval has too many roots"
  |otherwise = case numRootsInInterval p (low, mid) of
  0 -> Algebraic p (mid, high)
  1 -> Algebraic p (low, mid)
  _ -> error "Original interval should contain exactly one root"
  where
    mid = (low + high) / 2
    rootsLeft = numRootsInInterval p (low, mid)
    rootsRight = numRootsInInterval p (mid, high)
    nRoots = rootsLeft + rootsRight

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
    _ -> intervalMul' (shrinkInterval a) (shrinkInterval b) pab
  where
    intAB = boundInt intA intB

intervalAdd :: Algebraic -> Algebraic -> Poly Rational -> (Rational, Rational)
intervalAdd a@(Algebraic _ (lowA, highA)) b@(Algebraic _ (lowB, highB)) pab
  -- | traceShow (pab, intAPlusB, a, b) False = undefined
  | otherwise =
  case numRootsInInterval pab intAPlusB of
    0 -> error "Should have at least one root"
    1 -> intAPlusB
    _ -> intervalAdd (shrinkInterval a) (shrinkInterval b) pab
  where
    intAPlusB = (lowA + lowB, highA + highB)

boundInt :: (Multiplicative a, Additive a, Ord a) => (a, a) -> (a, a) -> (a, a)
boundInt intA@(lowA, highA) intB@(lowB, highB) = case sa of
  Neg -> case sb of
    Neg -> (highA * highB, lowA * lowB)
    Pos -> (lowA * highB, lowB * highA)
  Pos -> case sb of
    Neg -> boundInt intB intA
    Pos -> (lowA * lowB, highA * highB)
  where
    sa = fromJust $ intSign intA
    sb = fromJust $ intSign intB

data Sign = Neg | Pos
  deriving (Eq)

intSign :: (Additive a, Ord a) => (a, a) -> Maybe Sign
intSign (low, high)
  | low >= zero = Just Pos
  | high <= zero = Just Neg
  | otherwise = Nothing

-- Transforms the polynomial p(x) into p((x - a) / diff) in order to be able
-- to use numRoots.
numRootsInInterval :: ( AddGroup a, MulGroup a, Ord a, Show a) => Poly a -> (a, a) -> Int
numRootsInInterval p (low, high)
  -- If the interval is a single point, then we can't scale it up to the range [0,1].
  -- Instead, we simply check the value of the polynomial in that point, and if
  -- the result is zero, then by definition it has a root there, otherwise it has no roots.
  -- | traceShow p'' False = undefined
  | diff == zero = if evalP p low == zero then 1 else 0
  | otherwise = numRootsInclusive p''
  where
    diff = high - low
    p' = fmap (\c -> P [c]) p
    p'' = evalP p' (P [low, diff])

-- TODO: QuickCheck properties for LUDecomp

shrinkTest :: Algebraic
shrinkTest = a + b
  where
    a = Algebraic (P [6, - 5, 1]) (21 % 10, 1000)
    b = Algebraic (P [8, -6, 1]) (1, 39 % 10)

--------------- QuickCheck for Algebraic numbers -----------------------

sumOfRationalsProp :: Rational -> Rational -> Property
sumOfRationalsProp a b = toAlgebraic a + toAlgebraic b === toAlgebraic (a + b)

sum2prop :: [Rational] -> Property
sum2prop xs = sum (map toAlgebraic xs) === toAlgebraic (sum xs)

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

testMtoPoly :: (Eq a, Show a, AddGroup a, Multiplicative a) => Matrix a -> Maybe (MyMatrix (Poly a), [(Poly a, Int)], Poly a)
testMtoPoly m = myUDecomp $ MyMatrix $ toLists mSubLambdaI
  where
    n = nrows m
    m' = fmap (\c -> P [c]) m
    lambdaI = scaleMatrix (identity n) xP
    mSubLambdaI = m' ^-^ lambdaI

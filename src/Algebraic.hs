{-# LANGUAGE InstanceSigs #-}
module Algebraic (
  Algebraic(Algebraic),
  fromPWAlgebraic,
  toAlgebraic
) where
import           Data.Foldable      (find)
import           Debug.Trace        (trace, traceShow, traceShowId)
import           DSLsofMath.Algebra (AddGroup (..), Additive (..),
                                     MulGroup ((/)), Multiplicative (..), (-),
                                     (^+))
import           DSLsofMath.PSDS    (Poly (P), evalP, xP)
import           MatrixBridge       (Matrix (nrows), detLU, elementwise,
                                     flatten, fromLists, identity, toLists)
import           Poly.PolyCmp       (numRoots)
import           Prelude            hiding (negate, (*), (+), (-), (/))

data Algebraic = Algebraic (Poly Rational) (Rational, Rational)
  deriving (Show)

fromPWAlgebraic :: (Real a) => (Poly a, (a, a)) -> Algebraic
fromPWAlgebraic (p, (low, high)) = Algebraic (fmap toRational p) (toRational low, toRational high)

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (-x, x) which is guaranteed to contain x'
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = Algebraic (P [negate x', one]) interval
  where
    x' = toRational x
    interval = if x' < 0 then (x', 0) else (0, x')

instance Additive Algebraic where
  zero :: Algebraic
  zero = toAlgebraic (zero :: Int)
  (+) :: Algebraic -> Algebraic -> Algebraic
  (+) = undefined

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
    | low > zero = GT
    | high < zero = LT
    | evalP c zero == zero = EQ
    | otherwise = case numRoots c' of
      0 -> LT
      1 -> GT
      _ -> error "Should only be able to have 0 or 1 roots"
    where
      Algebraic c (low, high) = a - b
      c' = transCoeff c high

-- Transforms a polynomial p(x) to p(x/factor).
transCoeff :: (MulGroup a) =>Poly a -> a -> Poly a
transCoeff (P coeffs) factor = P $ go one coeffs
  where
    go _ []                    = []
    go factorAcc (coeff : coeffs') = (coeff / factorAcc) : go (factorAcc * factor) coeffs'

-- TODO add QuickCheck properties to test the implementation

mulAlgebraic :: Algebraic -> Algebraic -> Algebraic
mulAlgebraic (Algebraic a (lowA, highA)) (Algebraic b (lowB, highB)) = Algebraic ab intAB
  where
    aMatrix = toCharacteristicMatrix a
    bMatrix = toCharacteristicMatrix b
    abMatrix = outerProduct aMatrix bMatrix
    ab = toCharacteristicPoly abMatrix
    intAB = shrinkInterval (min lowA lowB, max highA highB) ab

-- Uses the companion matrix
toCharacteristicMatrix :: (AddGroup a, Eq a, MulGroup a) => Poly a -> Matrix a
toCharacteristicMatrix p = fromLists $ zipWith (createRow (n - 1)) [-1 ..] $ init coeffs'
  where
    (P coeffs') = toMonic p
    n = length coeffs'

toMonic :: (Eq a, Additive a, MulGroup a) => Poly a -> Poly a
toMonic (P coeffs)
  | lastCoeff == one = P coeffs
  | otherwise = P $ map (/ lastCoeff) coeffs
  where
    lastCoeff = last $ dropZeroes coeffs

dropZeroes :: (Eq a, Additive a) => [a] -> [a]
dropZeroes [] = []
dropZeroes (n : ns)
  | n == zero = case dropZeroes ns of
    []  -> []
    ns' -> n : ns'
  | otherwise = n : dropZeroes ns

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

shrinkInterval :: (a, a) -> Poly a -> (a, a)
shrinkInterval int _ = int

newtype MyMatrix a = MyMatrix [[a]]

instance (Show a) => Show (MyMatrix a) where
  show :: MyMatrix a -> String
  show (MyMatrix rows) = unlines $ map show rows

myUDet :: (MulGroup a, AddGroup a, Eq a, Show a) => MyMatrix a -> a
myUDet m = case myUDecomp m of
  Nothing        -> zero
  Just (u, f, s) -> (s * myDiagProd u) / f

myDiagProd :: Multiplicative a => MyMatrix a -> a
myDiagProd (MyMatrix rows) = go 0
  where
    n = length rows
    go i
      | i >= n = one
      | otherwise = (rows !! i !! i) * go (i + 1)


myUDecomp :: (Multiplicative a, AddGroup a, Eq a, Show a) => MyMatrix a -> Maybe (MyMatrix a, a, a)
myUDecomp m@(MyMatrix rows) = myUDecomp' m 0 (length rows) one one

-- TODO add comments describing the steps
-- as well as the purpose of moving multiplications to a separate variable
myUDecomp' :: (AddGroup a, Eq a, Multiplicative a, Show a) => MyMatrix a -> Int -> Int -> a -> a -> Maybe (MyMatrix a, a, a)
myUDecomp' u@(MyMatrix rows) i ncols f s
  -- | traceShow f False = undefined
  | i >= ncols = Just (u, f, s)
  | otherwise = do
      pivotIndex <- fmap fst $ find (\(_, row) -> row !! i /= zero) $ drop i $ zip [0 ..] rows
      let s' = if pivotIndex == i then s else negate s
      let u'@(MyMatrix rows') = swapRows u i pivotIndex
      let pivotElement = (rows' !! i) !! i -- Could be done in an earlier step
      let f' = f * (pivotElement ^+ (ncols - i - 1))
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

-- TODO: QuickCheck properties for LUDecomp

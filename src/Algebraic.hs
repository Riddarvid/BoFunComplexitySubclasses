{-# LANGUAGE InstanceSigs #-}
module Algebraic (
  fromPWAlgebraic,
  toAlgebraic
) where
import           DSLsofMath.Algebra         (AddGroup (..), Additive (..),
                                             MulGroup ((/)),
                                             Multiplicative (..), (-))
import           DSLsofMath.PSDS            (Poly (P), evalP)
import           Numeric.LinearAlgebra      (kronecker)
import           Numeric.LinearAlgebra.Data (Matrix)
import           Poly.PolyCmp               (numRoots)
import           Prelude                    hiding (negate, (*), (-), (/))

data Algebraic = Algebraic (Poly Rational) (Rational, Rational)

fromPWAlgebraic :: (Real a) => (Poly a, (a, a)) -> Algebraic
fromPWAlgebraic (p, (low, high)) = Algebraic (fmap toRational p) (toRational low, toRational high)

-- Converts a rational number to an algebraic number
-- Simply uses the polynomial p(x) = x - x'
-- and the interval (-x, x) which is guaranteed to contain x'
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = Algebraic (P [x', one]) interval
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
  negate (Algebraic (P coeffs) (low, high)) = Algebraic (P $ flipOdd coeffs) (-high, -low)

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
mulAlgebraic (Algebraic a intA) (Algebraic b intB) = undefined
  where
    aMatrix = toCharacteristicMatrix a
    bMatrix = toCharacteristicMatrix b
    abMatrix = undefined --kronecker aMatrix bMatrix

toCharacteristicMatrix :: Poly a -> Matrix Integer
toCharacteristicMatrix = undefined

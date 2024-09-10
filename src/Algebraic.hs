{-# LANGUAGE InstanceSigs #-}
module Algebraic (
  fromPWAlgebraic,
  toAlgebraic
) where
import           DSLsofMath.Algebra (AddGroup (..), Additive (..),
                                     Multiplicative (..), (-))
import           DSLsofMath.PSDS    (Poly (P), evalP)
import           Prelude            hiding (negate, (-))

data Algebraic = Algebraic (Poly Rational) (Rational, Rational)

fromPWAlgebraic :: (Real a) => (Poly a, (a, a)) -> Algebraic
fromPWAlgebraic (p, (low, high)) = Algebraic (fmap toRational p) (toRational low, toRational high)

-- Converts a rational number to a dyadic number
-- TODO
toAlgebraic :: (Real a) => a -> Algebraic
toAlgebraic x = undefined

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
  (*) = undefined

instance Eq Algebraic where
  (==) :: Algebraic -> Algebraic -> Bool
  a == b = let
    Algebraic c (low, high) = a - b
    in low < zero && zero < high && evalP c zero == zero

instance Ord Algebraic where
  compare :: Algebraic -> Algebraic -> Ordering
  compare a b
    | low > zero = GT
    | high < zero = LT
    | evalP c zero == zero = EQ
    | otherwise = undefined -- TODO count roots in (low, zero). If 1, then LT, otherwise GT.
    where
      Algebraic c (low, high) = a - b

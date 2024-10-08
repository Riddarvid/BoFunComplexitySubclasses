{-# OPTIONS_GHC -w #-} -- Code not central to the work, just used as library
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
module DSLsofMath.Algebra where
import           Data.Complex
import qualified Data.Ratio
import           Prelude      (Bool (..), Double, Foldable (foldr), Int,
                               Integer, Rational, const, error, otherwise, (.),
                               (<), (==))
import qualified Prelude

-------------------------------
-- Classes
infixl 6 -
infixl 6 +

infixl 7 *
infixl 7 /

class Additive a where
  zero :: a
  (+) :: a -> a -> a

sum :: (Foldable t, Additive a) => t a -> a
sum = foldr (+) zero

times :: Additive a => Integer -> a -> a
times n0 = if n0 < 0  then  error "Algebra.Classes.times: negative number of times"
                      else  go n0
  where go 0 _ = zero
        go 1 x = x
        go n x = if r == 0 then twoy else x + twoy
          where (m,r) = n `Prelude.divMod` 2
                y = go m x
                twoy = y+y

(-) :: AddGroup a => a -> a -> a
x - y           =  x + negate y

class Additive a => AddGroup a where
  negate :: a -> a

mult :: AddGroup a => Integer -> a -> a
mult n x = if n < 0 then negate (times (negate n) x) else times n x

class Multiplicative a where
  one :: a
  (*) :: a -> a -> a

product :: (Foldable t, Multiplicative a) => t a -> a
product = foldr (*) one

two :: (Additive a, Multiplicative a) => a
two = one+one

(^+) :: Multiplicative a => a -> Int -> a

x0 ^+ n0 = if n0 < 0  then  error "Algebra.Classes.^: negative exponent"
                      else  go n0 x0
  where go 0 _ = one
        go 1 x = x
        go n x = if r == 0 then y2 else x * y2
          where  (m,r) = n `Prelude.divMod` 2
                 y = go m x
                 y2 = y * y

type Ring a = (AddGroup a, Multiplicative a)

fromInteger :: Ring a => Integer -> a
fromInteger n = mult n one
fromIntegral :: (Prelude.Integral a, Ring b) => a -> b
fromIntegral = fromInteger . Prelude.toInteger

class Multiplicative a => MulGroup a where
  {-# MINIMAL (recip | (/)) #-}
  recip :: a -> a
  recip x         =  one / x

  (/) :: a -> a -> a
  x / y           =  x * recip y

(^) :: MulGroup a => a -> Int -> a
a ^ b | b < 0      = recip (a ^+ (negate b))
      | otherwise  = (a ^+ b)

type Field a = (Ring a, MulGroup a)

fromRational :: Field a => Data.Ratio.Ratio Integer -> a
fromRational x  =  fromInteger (Data.Ratio.numerator x) / fromInteger (Data.Ratio.denominator x)

class Field a => Algebraic a where
  sqrt :: a -> a

-- normally it should be "Algebraic" instead of "Field" but we're lazy like that.
--   (Also Transcendental is a terrible name; taken from the "numeric prelude".)
class Field a => Transcendental a where
  pi  :: a
  exp :: a -> a
  sin :: a -> a
  cos :: a -> a

cosh, sinh :: Transcendental a => a -> a
cosh x = (exp x + exp (negate x))/two
sinh x = (exp x - exp (negate x))/two

---------------------------------
-- Instances

instance Additive        Int       where  (+)  = (Prelude.+);  zero  = 0
instance Additive        Integer   where  (+)  = (Prelude.+);  zero  = 0
instance Additive        Rational  where  (+)  = (Prelude.+);  zero  = 0
instance Additive        Double    where  (+)  = (Prelude.+);  zero  = 0

instance AddGroup        Int       where  negate = Prelude.negate
instance AddGroup        Integer   where  negate = Prelude.negate
instance AddGroup        Rational  where  negate = Prelude.negate
instance AddGroup        Double    where  negate = Prelude.negate

instance Multiplicative  Int       where  (*)  = (Prelude.*);  one   = 1
instance Multiplicative  Integer   where  (*)  = (Prelude.*);  one   = 1
instance Multiplicative  Rational  where  (*)  = (Prelude.*);  one   = 1
instance Multiplicative  Double    where  (*)  = (Prelude.*);  one   = 1

instance MulGroup        Rational  where  (/)  = (Prelude./);  recip = Prelude.recip
instance MulGroup        Double    where  (/)  = (Prelude./);  recip = Prelude.recip

lift0 ::  a         ->  (x->a)
lift1 :: (a->b)     ->  (x->a) -> (x->b)
lift2 :: (a->b->c)  ->  (x->a) -> (x->b) -> (x->c)
lift0 = const
lift1 = (.)
lift2 op2 f g = \x -> op2 (f x) (g x)

instance Additive a        => Additive        (x -> a) where (+)  = lift2 (+);  zero  = lift0 zero
instance Multiplicative a  => Multiplicative  (x -> a) where (*)  = lift2 (*);  one   = lift0 one

instance AddGroup a   => AddGroup   (x -> a)  where   negate  =  lift1 negate
instance MulGroup a   => MulGroup   (x -> a)  where   recip   =  lift1 recip
instance Algebraic a  => Algebraic  (x -> a)  where   sqrt    =  lift1 sqrt

instance Transcendental a => Transcendental (x -> a) where
   pi = lift0 pi;  sin = lift1 sin;  cos = lift1 cos;  exp = lift1 exp

instance Algebraic       Double where   sqrt = Prelude.sqrt
instance Transcendental  Double where
   pi = Prelude.pi;  sin = Prelude.sin;  cos = Prelude.cos;  exp = Prelude.exp

instance Additive a  => Additive        (Complex a) where  (+) = addC;  zero  = zeroC
instance Ring a      => Multiplicative  (Complex a) where  (*) = mulC;  one   = oneC
instance AddGroup a  => AddGroup        (Complex a) where  negate  = negateC
instance Field a     => MulGroup        (Complex a) where  recip   = recipC

addC :: Additive a => Complex a -> Complex a -> Complex a
addC (x :+ y) (x' :+ y') = (x + x') :+ (y+y')

negateC :: AddGroup a => Complex a -> Complex a
negateC (a :+ b) = negate a :+ negate b

mulC :: Ring a => Complex a -> Complex a -> Complex a
mulC (a :+ b) (a' :+ b') = (a * a' - b * b') :+ (a * b' + b * a')

toC :: Additive a => a -> Complex a
toC x = x :+ zero

zeroC :: Additive a => Complex a
zeroC = toC zero

oneC :: (Additive a, Multiplicative a) => Complex a
oneC = toC one

recipC (a :+ b) = (a / m)  :+ (negate b / m)
  where m = a*a + b*b

instance (Algebraic a, Prelude.RealFloat a) =>  Algebraic (Complex a) where
  sqrt = Prelude.sqrt

instance (Transcendental a) => Transcendental (Complex a) where
  pi = piC;  exp = expC;  sin = sinC;  cos = cosC

piC :: Transcendental a => Complex a
piC = toC pi

expC, sinC, cosC, sinhC, coshC :: Transcendental a => Complex a -> Complex a
expC   (x:+y)  =  expx  * cos y  :+ expx * sin y  where expx = exp x
sinC   (x:+y)  =  sin x * cosh y :+ cos x * sinh y
cosC   (x:+y)  =  cos x * cosh y :+ negate (sin x * sinh y)

sinhC  (x:+y)  =  cos y * sinh x :+ sin y * cosh x
coshC  (x:+y)  =  cos y * cosh x :+ sin y * sinh x

---------------------------------
-- For RebindableSyntax

ifThenElse :: Bool -> p -> p -> p
ifThenElse c a b = if c then a else b

-------------------------------
-- Typesetting aliases.

neg :: AddGroup a => a -> a
neg = negate   -- |neg| is used to typeset unary minus as a shorter dash, closer to its argument

frac :: MulGroup a => a -> a -> a
frac = (/)     -- |frac| is used to typeset a fraction (more compactly than |x / y|)

-------------------------------
-- Christian

square :: (Ring a) => a -> a
square x = x * x

{- Unused dyadic stuff.

-- We don't normalize.
data Dyadic a = Dyadic Int a

instance (Additive a, Multiplicative a) => Additive (Dyadic a) where
  zero = Dyadic 0 zero

  Dyadic d_0 k_0 + Dyadic d_1 k_1 = Dyadic d k
    where
    d = Prelude.max d_0 d_1
    k_0' = k_0 * (two ^+ (d - d_0))
    k_1' = k_1 * (two ^+ (d - d_1))
    k = k_0' + k_1'

instance (Ring a) => AddGroup (Dyadic a) where
  negate (Dyadic d k) = Dyadic d (negate k)

instance (Ring a, Prelude.Eq a) => Prelude.Eq (Dyadic a) where
  a == b = k == zero where
    Dyadic _ k = a - b

instance (Ring a, Prelude.Ord a) => Prelude.Ord (Dyadic a) where
  compare a b = Prelude.compare k zero where
    Dyadic _ k = a - b
-}

class (Prelude.Eq a, AddGroup a, Multiplicative a) => Euclidean a where
  quotRem :: a -> a -> (a, a)

instance Euclidean Integer where
  quotRem = Prelude.quotRem

quot :: Euclidean a => a -> a -> a
quot x y = q where
  (q, _) = quotRem x y

rem :: Euclidean a => a -> a -> a
rem x y = r where
  (_, r) = quotRem x y

-- Finds a generator for the ideal generated by the given elements.
-- Also known as the extended GCD.
generator :: Euclidean a => (a, a) -> (a, (a, a))
generator (x, y) | y == zero = (x, (one, zero))
                   | otherwise = (r, (c, b - c * q)) where
    (q, z) = quotRem x y
    (r, (b, c)) = generator (y, z)

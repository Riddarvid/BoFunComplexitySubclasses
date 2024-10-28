{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.Iterated (
  Iterated,
  IteratedSymm,
  liftIter,
  idIter,
  iterateFun,
  iterateSymmFun
) where
import           ArbitraryArity        (ArbitraryArity (arbitraryArity))
import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Enumerable    (Enumerable, Shared, Sized (aconcat),
                                        Typeable, c1, share)
import           Control.Monad.Free    (Free (..))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoize)
import           Data.Functor.Classes  (Eq1)
import           Data.Hashable         (Hashable)
import qualified Data.MultiSet         as MultiSet
import           Subclasses.Lifted     (Lifted, LiftedSymmetric, liftFun,
                                        liftFunSymm)
import           Test.Feat             (enumerate)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, chooseInt,
                                        frequency, oneof, sized)

type Iterated'' f g = Free f g

type Iterated' f = Iterated'' f ()

type Iterated f = Iterated' (Lifted f)

type IteratedSymm f = Iterated' (LiftedSymmetric f)

-------------------------------------------

type IterStep' f g = f (Iterated'' f g)

type IterStep f = f (Iterated' f)

--------------------------------------------

instance (Eq1 f, Hashable g, Hashable (f (Iterated'' f g))) => Hashable (Iterated'' f g)

instance (BoFun (IterStep f) (i, [i]), Constable (IterStep f)) => BoFun (Iterated' f) [i] where
  isConst :: Iterated' f -> Maybe Bool
  isConst (Pure ()) = Nothing
  isConst (Free u)  = isConst u

  variables :: Iterated' f -> [[i]]
  variables (Pure ()) = [[]]
  variables (Free v)  = variables v & map (uncurry (:))

  setBit :: ([i], Bool) -> Iterated' f -> Iterated' f
  setBit ([], val)     (Pure _) = Free $ mkConst val
  setBit _             (Pure _) = error "Too many levels"
  setBit (i : is, val) (Free v) = Free $ setBit ((i, is), val) v
  setBit ([], _)       (Free _) = error "Too few levels"

-- f will likely be memoizable over all types of subfunction,
-- but here we only need it to be memoizable over specifically f (Iterated f)
instance (Memoizable (IterStep' f g), Memoizable g) => Memoizable (Iterated'' f g) where
  memoize :: (Iterated'' f g -> v) -> Iterated'' f g -> v
  memoize = $(deriveMemoize ''Free)

-- TODO-NEW: This probably works as a general instance, but we should also look at
-- something using bit-number.
instance (Enumerable g, Enumerable (IterStep' f g), Typeable f) =>
  Enumerable (Iterated'' f g) where

  enumerate :: (Typeable enum, Sized enum) => Shared enum (Iterated'' f g)
  enumerate = share $ aconcat [
    c1 Pure,
    c1 Free]

instance (Constable (IterStep' f g)) => Constable (Iterated'' f g) where
  mkConst :: Bool -> Iterated'' f g
  mkConst = Free . mkConst

-- Size is used to determine where the tree should end
instance (ArbitraryArity (IterStep f)) => Arbitrary (Iterated' f) where
  arbitrary :: Gen (Iterated' f)
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    if n' == 0 then return (Pure ()) else arbitraryArity n'

instance (ArbitraryArity (IterStep f)) => ArbitraryArity (Iterated' f) where
  arbitraryArity :: Int -> Gen (Iterated' f)
  arbitraryArity 1     = oneof [return $ Pure (), Free <$> arbitraryArity 1]
  arbitraryArity arity = Free <$> arbitraryArity arity

liftIter :: f (Iterated' f) -> Iterated' f
liftIter = Free

idIter :: Iterated' f
idIter = Pure ()

-- The consumer must ensure that f is symmetric
iterateSymmFun :: Ord f => Int -> f -> Int -> IteratedSymm f
iterateSymmFun bits f n'
  | n' <= 0 = error "Number of levels must be >= 1"
  | bits <= 0 = error "Number of input bits must be >= 1"
  | otherwise = go n'
  where
    go 0 = idIter
    go n = liftIter f'
      where
        subFun = go (n - 1)
        f' = liftFunSymm f $ MultiSet.fromOccurList [(subFun, bits)]

iterateFun :: Int -> f -> Int -> Iterated f
iterateFun bits f = go
  where
    go 0 = idIter
    go n = liftIter f'
      where
        subFun = iterateFun bits f (n - 1)
        f' = liftFun f $ replicate bits subFun

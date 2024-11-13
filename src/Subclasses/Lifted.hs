{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Subclasses.Lifted (
  LiftedSymmetric,
  liftFunSymm
) where
import           Arity                 (AllArity (allArity),
                                        ArbitraryArity (arbitraryArity))
import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Arrow         ((>>>))
import           Control.DeepSeq       (NFData)
import           Data.Function.Memoize (Memoizable, memoize)
import           Data.Functor.Classes  (Eq1 (liftEq), Eq2 (liftEq2),
                                        Ord1 (liftCompare), Ord2 (liftCompare2),
                                        Show1 (liftShowsPrec), showsBinaryWith)
import           Data.MultiSet         (MultiSet)
import qualified Data.MultiSet         as MultiSet
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           GHC.Generics          (Generic)
import           Test.QuickCheck       (Gen, elements)
import           Utils                 (generatePartition, naturals, partitions)



-- Code for the non-symmetric version of Lifted. Will probably be useful in the future, but right
-- now it leads to a lot of almost duplicated code.
{-
-- Invariant: g only contains non-const functions
-- TODO-NEW: "smart constructor" ensuring this.
data Lifted f g = Lifted {
  lFun     :: f,
  lSubFuns :: [g]
}

-- TODO-NEW handle constant g's
liftFun :: f -> [g] -> Lifted f g
liftFun = Lifted

-- TODO-NEW Normalized version where setBit, in the case that the entire function becomes
-- constant, replaces the function with a 0-ary const function. Requires Constable.
newtype NormalizedLifted f g = NL (Lifted f g)


instance (BoFun f Int, BoFun g j) => BoFun (Lifted f g) (Int, j) where
  isConst :: Lifted f g -> Maybe Bool
  isConst = isConst . lFun
  variables :: Lifted f g -> [(Int, j)]
  variables lf = do
    (subFun, i) <- zip (lSubFuns lf) naturals
    j <- variables subFun
    return (i, j)
  setBit :: ((Int, j), Bool) -> Lifted f g -> Lifted f g
  setBit ((i, j), v) lf = case isConst subFun' of
    Nothing -> lf{lSubFuns = start ++ (subFun : end)}
    Just v' -> lf{lFun = setBit (i, v') $ lFun lf, lSubFuns = start ++ end}
    where
      (start, subFun, end) = fromJust $ splitList i $ lSubFuns lf
      subFun' = setBit (j, v) subFun

splitList :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs = case end of
  []        -> Nothing
  (x: end') -> Just (start, x, end')
  where
    (start, end) = splitAt n xs
-}

data LiftedSymmetric f g = LiftedSymmetric {
  lsFun     :: f,
  lsSubFuns :: MultiSet g
} deriving(Eq, Ord, Generic)

instance (NFData f, NFData g) => NFData (LiftedSymmetric f g)

-- TODO-NEW handle constant g's
liftFunSymm :: f -> MultiSet g -> LiftedSymmetric f g
liftFunSymm = LiftedSymmetric

-- TODO-NEW: Figure out how to derive AllArity
-- Currently can't coerce.
-- This type probably only grants a very small efficiency improvement, removing
-- some equivalent functions.
-- Might even give worse performance as we have to do a lot more isConst calculations.
newtype NormalizedLiftedSymmetric f g = NLS (LiftedSymmetric f g)
  deriving (Eq1, Ord1, Show1, Memoizable, Constable, ArbitraryArity)

instance (BoFun f (), BoFun g j, Ord g, Constable f) =>
  BoFun (NormalizedLiftedSymmetric f g) (Int, j) where
  isConst :: NormalizedLiftedSymmetric f g -> Maybe Bool
  isConst (NLS f) = isConst f
  variables :: NormalizedLiftedSymmetric f g -> [(Int, j)]
  variables (NLS f) = variables f
  setBit :: ((Int, j), Bool) -> NormalizedLiftedSymmetric f g -> NormalizedLiftedSymmetric f g
  setBit v (NLS f) = case isConst f' of
    Nothing  -> NLS f'
    Just res -> mkConst res
    where
      f' = setBit v f

-- Note that even though the lifted function is symmetric, and therefore has variable type (),
-- the resulting lifted function will have variable type (Int, j). This is because we still
-- need a way to determine which subfunction a setBit call should be applied to, since
-- the subfunctions don't have any restrictions on them.

-- The first value in the variable tuple is 0-indexed.
instance (BoFun f (), BoFun g j, Ord g) => BoFun (LiftedSymmetric f g) (Int, j) where
  isConst :: LiftedSymmetric f g -> Maybe Bool
  isConst = isConst . lsFun
  variables :: LiftedSymmetric f g -> [(Int, j)]
  variables lsf = do
    ((subFun, _), i) <- zip (MultiSet.toAscOccurList (lsSubFuns lsf)) naturals
    j <- variables subFun
    return (i, j)
  setBit :: ((Int, j), Bool) -> LiftedSymmetric f g -> LiftedSymmetric f g
  setBit ((i, j), v) lsf = case isConst subFun' of
    Nothing -> lsf{lsSubFuns = MultiSet.insert subFun' subFuns'}
    Just v' -> lsf{lsFun = setBit ((), v') $ lsFun lsf, lsSubFuns = subFuns'}
    where
      subFuns = lsSubFuns lsf
      (subFun, _) = MultiSet.toAscOccurList subFuns !! i
      subFuns' = MultiSet.delete subFun subFuns
      subFun' = setBit (j, v) subFun

instance (Memoizable f, Memoizable g, Ord g) => Memoizable (LiftedSymmetric f g) where
  memoize :: (LiftedSymmetric f g -> v) -> LiftedSymmetric f g -> v
  memoize f = toTuple >>> memoize (fromTuple >>> f) where
    toTuple (LiftedSymmetric fun subFuns) = (fun, subFuns)
    fromTuple (t, us) = LiftedSymmetric t us

instance (Eq f) => Eq1 (LiftedSymmetric f) where
  liftEq :: (a -> b -> Bool) -> LiftedSymmetric f a -> LiftedSymmetric f b -> Bool
  liftEq eq' (LiftedSymmetric t us) (LiftedSymmetric t' us') =
    liftEq2 (==) (liftEq eq') (t, us) (t', us')

instance (Ord f) => Ord1 (LiftedSymmetric f) where
  liftCompare :: (a -> b -> Ordering) -> LiftedSymmetric f a -> LiftedSymmetric f b -> Ordering
  liftCompare compare' (LiftedSymmetric t us) (LiftedSymmetric t' us') =
    liftCompare2 compare (liftCompare compare') (t, us) (t', us')

instance (Show f) => Show1 (LiftedSymmetric f) where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> LiftedSymmetric f a -> ShowS
  liftShowsPrec showsPrec' showList' p (LiftedSymmetric t u) =
    showsBinaryWith showsPrec (liftShowsPrec showsPrec' showList') "LiftedSymmetric" p t u

instance (Constable f) => Constable (LiftedSymmetric f g) where
  mkConst :: Bool -> LiftedSymmetric f g
  mkConst val = LiftedSymmetric (mkConst val) MultiSet.empty

instance (ArbitraryArity f, ArbitraryArity g, Ord g, Constable f) =>
  ArbitraryArity (LiftedSymmetric f g) where
  arbitraryArity :: Int -> Gen (LiftedSymmetric f g)
  arbitraryArity 0 = elements [mkConst False, mkConst True]
  arbitraryArity arity = do
    (gs, nSubFuns) <- generateSubFuns arity
    f <- arbitraryArity nSubFuns
    return $ LiftedSymmetric {lsFun = f, lsSubFuns = gs}

generateSubFuns :: (ArbitraryArity f, Ord f) => Int -> Gen (MultiSet f, Int)
generateSubFuns totalArity = do
  partition <- generatePartition totalArity
  subFuns <- mapM arbitraryArity partition
  return (MultiSet.fromList subFuns, length subFuns)

----------- Generate all ------------------

instance (Ord f, Ord g, AllArity g, AllArity f) => AllArity (LiftedSymmetric f g) where
  allArity :: Int -> Set (LiftedSymmetric f g)
  allArity n = Set.fromList $ do
    partition <- partitions n
    subFuns <- mapM (Set.toList . allArity) partition
    f <- Set.toList $ allArity $ length subFuns
    return $ LiftedSymmetric {lsFun = f, lsSubFuns = MultiSet.fromList subFuns}

----------- Enumeration --------------------

-- TODO-NEW: Right now it feels like we won't get much value out of enumerate,
-- so we will focus our efforts elsewhere.
{-enumerateNArySymm :: (Typeable f1, Sized f1, EnumerateArity g) => Int -> Shareable f1 (LiftedSymmetric f g)
enumerateNArySymm = help


help :: (Typeable f1, Sized f1) => Int -> Shareable f1 (LiftedSymmetric f g)
help n = () <$> enumerateArity ()
  where
    subEnum = aconcat $ map buildLiftedSymm $ partitions n

buildLiftedSymm :: Partition Int -> Shareable f1 ([g])
buildLiftedSymm []  = undefined
buildLiftedSymm [n] = enumerateArity n-}




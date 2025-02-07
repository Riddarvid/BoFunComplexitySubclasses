{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PatternSynonyms       #-}
module Subclasses.MultiComposed.MultiComposed (
  Lifted(Lifted),
) where
import           Arity                      (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun           (BoFun (..), shrinkBoFun)
import           Control.DeepSeq            (NFData)
import           Data.Maybe                 (fromJust)
import           Exploration.PrettyPrinting (PrettyBoFun (prettyShow))
import           GHC.Generics               (Generic)
import           Test.QuickCheck            (Arbitrary (shrink), Gen, chooseInt,
                                             sized)
import           Test.QuickCheck.Arbitrary  (arbitrary)
import           Utils                      (generatePartition, indent,
                                             naturals)

-- Invariant: g only contains non-const functions
data Lifted f g = Lifted' {
  lFun     :: f,
  lSubFuns :: [g]
} deriving (Generic, Show, Read)

pattern Lifted :: (BoFun f Int, BoFun g i) => f -> [g] -> Lifted f g
pattern Lifted f gs <- Lifted' f gs where
  Lifted f gs = toLifted f gs

toLifted :: (BoFun f Int, BoFun g i) => f -> [g] -> Lifted f g
toLifted f = reduceConstants . Lifted' f

instance (PrettyBoFun f, PrettyBoFun g) => PrettyBoFun (Lifted f g) where
  prettyShow :: Lifted f g -> String
  prettyShow (Lifted' f gs) =
    prettyShow f ++ "\n" ++
    indent (showSubFuns gs)

showSubFuns :: PrettyBoFun g => [g] -> String
showSubFuns = unlines . map prettyShow

instance (NFData f, NFData g) => NFData (Lifted f g)

{-instance (Memoizable f, Memoizable g) => Memoizable (Lifted f g) where
  memoize :: (Lifted f g -> v) -> Lifted f g -> v
  memoize f = destruct >>> memoize (construct >>> f)
    where
      destruct (Lifted' f' gs) = (f', gs)
      construct (f', gs) = Lifted' f' gs-}

reduceConstants :: (BoFun f Int, BoFun g i) => Lifted f g -> Lifted f g
reduceConstants (Lifted' f gs) = case v of
  Nothing      -> Lifted' f gs
  Just (i, v') -> reduceConstants $ Lifted' (setBit (i, v') f) (deleteAt i gs)
  where
    v = firstConst $ zip naturals $ map isConst gs

firstConst :: [(Int, Maybe a)] -> Maybe (Int, a)
firstConst [] = Nothing
firstConst ((i, v) : vs) = case v of
  Nothing -> firstConst vs
  Just v' -> Just (i, v')

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
    Nothing -> lf{lSubFuns = start ++ (subFun' : end)}
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

deleteAt :: Int -> [a] -> [a]
deleteAt _ []       = []
deleteAt 0 (_ : xs) = xs
deleteAt n (_ : xs) = deleteAt (n - 1) xs

instance (ArbitraryArity f, ArbitraryArity g, BoFun f Int, BoFun g j) => Arbitrary (Lifted f g) where
  arbitrary :: Gen (Lifted f g)
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    arbitraryArity n'
  shrink :: Lifted f g -> [Lifted f g]
  shrink = shrinkBoFun

instance (ArbitraryArity f, ArbitraryArity g) => ArbitraryArity (Lifted f g) where
  arbitraryArity :: Int -> Gen (Lifted f g)
  arbitraryArity arity = do
    (gs, nSubFuns) <- generateSubFuns arity
    f <- arbitraryArity nSubFuns
    return $ Lifted' {lFun = f, lSubFuns = gs}

generateSubFuns :: (ArbitraryArity f) => Int -> Gen ([f], Int)
generateSubFuns totalArity = do
  partition <- generatePartition totalArity
  subFuns <- mapM arbitraryArity partition
  return (subFuns, length subFuns)

-- data LiftedSymmetric f g = LiftedSymmetric {
--   lsFun     :: f,
--   lsSubFuns :: MultiSet g
-- } deriving(Eq, Ord, Generic)

-- instance (NFData f, NFData g) => NFData (LiftedSymmetric f g)

-- liftFunSymm :: f -> MultiSet g -> LiftedSymmetric f g
-- liftFunSymm = LiftedSymmetric
-- -- Note that even though the lifted function is symmetric, and therefore has variable type (),
-- -- the resulting lifted function will have variable type (Int, j). This is because we still
-- -- need a way to determine which subfunction a setBit call should be applied to, since
-- -- the subfunctions don't have any restrictions on them.

-- -- The first value in the variable tuple is 0-indexed.
-- instance (BoFun f (), BoFun g j, Ord g) => BoFun (LiftedSymmetric f g) (Int, j) where
--   isConst :: LiftedSymmetric f g -> Maybe Bool
--   isConst = isConst . lsFun
--   variables :: LiftedSymmetric f g -> [(Int, j)]
--   variables lsf = do
--     ((subFun, _), i) <- zip (MultiSet.toAscOccurList (lsSubFuns lsf)) naturals
--     j <- variables subFun
--     return (i, j)
--   setBit :: ((Int, j), Bool) -> LiftedSymmetric f g -> LiftedSymmetric f g
--   setBit ((i, j), v) lsf = case isConst subFun' of
--     Nothing -> lsf{lsSubFuns = MultiSet.insert subFun' subFuns'}
--     Just v' -> lsf{lsFun = setBit ((), v') $ lsFun lsf, lsSubFuns = subFuns'}
--     where
--       subFuns = lsSubFuns lsf
--       (subFun, _) = MultiSet.toAscOccurList subFuns !! i
--       subFuns' = MultiSet.delete subFun subFuns
--       subFun' = setBit (j, v) subFun

-- instance (Memoizable f, Memoizable g, Ord g) => Memoizable (LiftedSymmetric f g) where
--   memoize :: (LiftedSymmetric f g -> v) -> LiftedSymmetric f g -> v
--   memoize f = toTuple >>> memoize (fromTuple >>> f) where
--     toTuple (LiftedSymmetric fun subFuns) = (fun, subFuns)
--     fromTuple (t, us) = LiftedSymmetric t us

-- instance (Eq f) => Eq1 (LiftedSymmetric f) where
--   liftEq :: (a -> b -> Bool) -> LiftedSymmetric f a -> LiftedSymmetric f b -> Bool
--   liftEq eq' (LiftedSymmetric t us) (LiftedSymmetric t' us') =
--     liftEq2 (==) (liftEq eq') (t, us) (t', us')

-- instance (Ord f) => Ord1 (LiftedSymmetric f) where
--   liftCompare :: (a -> b -> Ordering) -> LiftedSymmetric f a -> LiftedSymmetric f b -> Ordering
--   liftCompare compare' (LiftedSymmetric t us) (LiftedSymmetric t' us') =
--     liftCompare2 compare (liftCompare compare') (t, us) (t', us')

-- instance (Show f) => Show1 (LiftedSymmetric f) where
--   liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> LiftedSymmetric f a -> ShowS
--   liftShowsPrec showsPrec' showList' p (LiftedSymmetric t u) =
--     showsBinaryWith showsPrec (liftShowsPrec showsPrec' showList') "LiftedSymmetric" p t u

-- instance (Constable f) => Constable (LiftedSymmetric f g) where
--   mkConst :: Bool -> LiftedSymmetric f g
--   mkConst val = LiftedSymmetric (mkConst val) MultiSet.empty

-- instance (ArbitraryArity f, ArbitraryArity g, Ord g, Constable f) =>
--   ArbitraryArity (LiftedSymmetric f g) where
--   arbitraryArity :: Int -> Gen (LiftedSymmetric f g)
--   arbitraryArity 0 = elements [mkConst False, mkConst True]
--   arbitraryArity arity = do
--     (gs, nSubFuns) <- generateSubFuns arity
--     f <- arbitraryArity nSubFuns
--     return $ LiftedSymmetric {lsFun = f, lsSubFuns = gs}

-- generateSubFuns :: (ArbitraryArity f, Ord f) => Int -> Gen (MultiSet f, Int)
-- generateSubFuns totalArity = do
--   partition <- generatePartition totalArity
--   subFuns <- mapM arbitraryArity partition
--   return (MultiSet.fromList subFuns, length subFuns)

----------- Generate all ------------------

-- instance (Ord f, Ord g, AllArity g, AllArity f) => AllArity (LiftedSymmetric f g) where
--   allArity :: Int -> Set (LiftedSymmetric f g)
--   allArity n = Set.fromList $ do
--     partition <- partitions n
--     subFuns <- mapM (Set.toList . allArity) partition
--     f <- Set.toList $ allArity $ length subFuns
--     return $ LiftedSymmetric {lsFun = f, lsSubFuns = MultiSet.fromList subFuns}



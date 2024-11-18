{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where

import           Control.Applicative   (Alternative (empty), liftA2)
import           Control.Arrow         ((&&&), (>>>))
import           Control.Enumerable    (Enumerable, Shareable, Sized (aconcat),
                                        Typeable, access)
import           Control.Monad         (forM_, unless)
import           Control.Monad.State   (MonadState (..), execState)
import           Data.Function.Memoize (Memoizable (..))
import           Data.Functor.Classes  (Eq1 (..), Eq2 (..), Ord1 (..),
                                        Ord2 (..), Show1 (..), Show2 (..))
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IM
import           Data.MultiSet         (MultiSet)
import qualified Data.MultiSet         as MultiSet
import qualified Data.Set              as Set
import           DSLsofMath.Algebra    (AddGroup, Additive (zero),
                                        Multiplicative (one))
import qualified DSLsofMath.Algebra    as A
import           System.Random         (Random)
import qualified Test.QuickCheck       as QC
import           Test.QuickCheck       (Gen)

-- Monoids.

fromJustMonoid :: Monoid a => Maybe a -> a
fromJustMonoid (Just x) = x
fromJustMonoid Nothing  = mempty

class (Monoid a) => Group a where
  invert :: a -> a


-- MultiSet instances.

instance (Ord a, Memoizable a) => Memoizable (MultiSet a) where
  memoize f = MultiSet.toAscOccurList >>> memoize (MultiSet.fromAscOccurList >>> f)

-- TODO: These should be in Data.MultiSet.
instance Eq1 MultiSet.MultiSet where
  liftEq eq' u v = liftEq (liftEq2 eq' (==)) u' v' where
    u' = MultiSet.toAscOccurList u
    v' = MultiSet.toAscOccurList v

instance Ord1 MultiSet.MultiSet where
  liftCompare compare' u v = liftCompare (liftCompare2 compare' compare) u' v' where
    u' = MultiSet.toAscOccurList u
    v' = MultiSet.toAscOccurList v

instance Show1 MultiSet.MultiSet where
  liftShowsPrec showsPrec' showList' p =
        MultiSet.toOccurList
    >>> liftShowList2 showsPrec' showList' showsPrec showList
    >>> (showString "fromOccurList " .)
    >>> showParen (p > 10)


-- Combinatorics.

naturals :: (Integral i) => [i]
naturals = iterate (+ 1) 0

-- Number of order-independent choices of k elements from n.
choose :: Integer -> Integer -> Integer
choose n k = if k == 0 then 1 else choose (n - 1) (k - 1) * n `quot` k

-- Number of order-independent repeated choices of k elements from n.
chooseMany :: Integer -> Integer -> Integer
chooseMany n k = choose (n + k - 1) k

-- Number of order-independent repeated choices of up to k elements from n.
chooseManyUpTo :: Integer -> Integer -> Integer
chooseManyUpTo n = chooseMany (n + 1)


-- Working with Bool.

type Square x = (x, x)

duplicate :: x -> Square x
duplicate = id &&& id

-- Universal property of Bool.
-- This is a natural isomorphism.
-- TODO: find shorter names, perhaps use Iso' from lens library.
tabulateBool :: (Bool -> x) -> Square x
tabulateBool f = (f True, f False)

lookupBool :: Square x -> (Bool -> x)
lookupBool (a, b) v = if v then a else b

mapPair :: (a -> b) -> Square a  -> Square b
mapPair f = lookupBool >>> (f .) >>> tabulateBool

squareToList :: Square x -> [x]
squareToList (a, b) = [a, b]

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe v x = if v then Just x else Nothing

unify :: (Eq a) => (a, a) -> Maybe a
unify (x, y) = boolToMaybe (x == y) x


-- Graph reachability.

dfs' :: (Ord a, MonadState (Set.Set a) m) => (a -> [a]) -> a -> m ()
dfs' outgoing = h where
  h a = do
    visited <- get
    unless (Set.member a visited) $ do
      put $ Set.insert a visited
      forM_ (outgoing a) h

dfs :: (Ord a) => (a -> [a]) -> a -> Set.Set a
dfs outgoing start = execState (dfs' outgoing start) Set.empty

------------------------------------------------------------------------

outputPermutations :: Int -> [[Bool]]
outputPermutations n = permutations (2^n)

permutations :: Int -> [[Bool]]
permutations 0 = [[]]
permutations n
  | n < 0 = error "n must be non-negative"
permutations n = do
  v <- [False, True]
  vs <- permutations (n - 1)
  return (v : vs)

type Partition a = [a]

-- The reason that we're dropping the last one is that
-- f == ThresholdFun (1,1) [f]. We are not creating a new function by simply wrapping it
-- in a ThresholdFun.
-- Does not generate any 0's.
partitions :: (Ord a, Enum a, AddGroup a, Multiplicative a) => a -> [Partition a]
partitions = partitions' one

-- Generate all partitions of n where a partition is a sorted list of numbers summing to n.
-- The first element in the partition must be >= highest.
partitions' :: (Ord a, Enum a, AddGroup a) => a -> a -> [Partition a]
partitions' highest n
  | n == zero = [[]]
  | highest > n = []
  | otherwise = concatMap (\m -> map (m :) $ partitions' m (n A.- m)) [highest .. n]

generatePartition :: (Random a, Ord a, Enum a, AddGroup a, Multiplicative a) => a -> Gen (Partition a)
generatePartition n
  | n == zero = return []
  | n == one = return [n]
  | otherwise = do
  m' <- QC.choose (one, n)
  let n' = n A.- m'
  p <- generatePartition n'
  return (m' : p)

newtype Partition' a = Partition' [a]

enumeratePartitions :: (Typeable f, Sized f, Ord a, Enum a, AddGroup a, Multiplicative a) => a -> Shareable f (Partition' a)
enumeratePartitions = enumeratePartitions' one

enumeratePartitions' :: (Ord a, Enum a, AddGroup a, Typeable f, Sized f) => a -> a -> Shareable f (Partition' a)
enumeratePartitions' highest n
  | n == zero = pure (Partition' [])
  | highest > n = empty
  | otherwise = subPartitions
  where
    subPartitions = aconcat $ map (\m -> (\(Partition' ms) -> Partition' (m : ms)) <$> enumeratePartitions' m (n A.- m)) [highest .. n]

------------------------------------------------------------------------

listToVarAssignment :: [Bool] -> IntMap Bool
listToVarAssignment xs = IM.fromAscList $ zip [1 ..] xs

--------------------------------------------------------------------

data Sign = Neg | Zero | Pos
  deriving (Eq, Show)

--------------------------------------------------------

-- Enumerates MultiSets with exactly n members
-- MultSets are free
enumerateMultiSet :: (Typeable f, Sized f, Ord a, Enumerable a) => Int -> Shareable f (MultiSet a)
enumerateMultiSet 0 = pure MultiSet.empty
enumerateMultiSet n = insertMultiSetF access $ enumerateMultiSet (n - 1)

insertMultiSetF :: (Applicative f, Ord a) => f a -> f (MultiSet a) -> f (MultiSet a)
insertMultiSetF = liftA2 MultiSet.insert

indent :: String -> String
indent = unlines . map ('\t' :) . lines

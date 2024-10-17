{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Subclasses.Threshold (
  Threshold(Threshold),
  ThresholdFun(ThresholdFun),
  majFun,
  iteratedFun,
  iteratedMajFun,
  iteratedThresholdFunConst,
  arityIteratedThreshold,
  allNAryITFs
) where

import           Control.Arrow         ((>>>))
import           Control.Monad.Free    (Free (..))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (..), deriveMemoizable)
import           Data.Functor.Classes  (Eq1 (..), Eq2 (..), Ord1 (..),
                                        Ord2 (..), Show1 (..), showsBinaryWith)
import qualified Data.MultiSet         as MultiSet
import           Prelude               hiding (negate, sum, (+), (-))

import           DSLsofMath.Algebra    (AddGroup (..), Additive (..), sum, (-))

import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Applicative   (Applicative (liftA2), (<|>))
import           Control.Enumerable    (Shareable, Shared, Sized (aconcat),
                                        access, c1, datatype, share)
import           Data.MultiSet         (MultiSet)
import           Subclasses.Iterated   (Iterated, Iterated')
import           Test.Feat             (Enumerable (enumerate))
import           Test.QuickCheck       (Arbitrary (arbitrary), chooseInt,
                                        elements, sized)
import           Test.QuickCheck.Gen   (Gen)
import           Type.Reflection       (Typeable)
import           Utils                 (Square, chooseMany, duplicate,
                                        lookupBool, naturals, partitions,
                                        squareToList, tabulateBool)

-- | A threshold for a Boolean function.
-- Number of inputs needed for 'True' and 'False' result, respectively.
-- The sum of these thresholds equals the number of inputs plus one.
-- Each threshold is non-negative.
newtype Threshold = Threshold (Square Int)
  deriving (Show, Eq, Ord)

$(deriveMemoizable ''Threshold)

-- Pointwise (via Bool -> Int).
-- TODO: Derive automatically?
instance Additive Threshold where
  zero = Threshold $ tabulateBool $ const zero
  Threshold a + Threshold b = Threshold $ tabulateBool $ \i -> lookupBool a i + lookupBool b i
-- TODO: perhaps generalise to some finite type instead of just Bool

instance AddGroup Threshold where
  negate (Threshold t) = Threshold $ tabulateBool $ lookupBool t >>> negate

-- TODO: What would be the type class for an algebra over the integers?
thresholdScale :: Int -> Threshold -> Threshold
thresholdScale x (Threshold t) = Threshold $ tabulateBool $ lookupBool t >>> (x *)

thresholdNumInputs :: Threshold -> Int
thresholdNumInputs (Threshold (nt, nf)) = nt + nf - 1

-- | Reachable excluding constant functions.
numReachable' :: [Threshold] -> Integer
numReachable' [] = 1
numReachable' ((Threshold v) : ts) = sum $ do
  let n = numReachable' ts
  let vs = squareToList v
  i <- take (sum vs) naturals
  let factor = vs & map (toInteger >>> subtract i >>> min 0) & sum & (+ i)
  return $ factor * chooseMany n i

{-
Number of Boolean functions reachable from 'iteratedThresholdFun ts' by setting variables.
That is:
>>> length $ reachable $ iteratedThresholdFun ts'
-}
numReachable :: [Threshold] -> Integer
numReachable = numReachable' >>> (+ 2)

-- A constant threshold (fixed result).
thresholdConst :: Bool -> Threshold
thresholdConst False = Threshold (1, 0)
thresholdConst True  = Threshold (0, 1)

thresholdIsConst :: Threshold -> Maybe Bool
thresholdIsConst (Threshold (nt, nf)) = if
  | nt <= 0   -> Just True
  | nf <= 0   -> Just False
  | otherwise -> Nothing

-- | A majority threshold.
thresholdMaj :: Int -> Threshold
thresholdMaj = duplicate >>> Threshold

-- | A threshold-type Boolean function.
data ThresholdFun f = ThresholdFun {
  threshold          :: Threshold,
  -- The subfunctions.
  -- None of the subfunctions are constant.
  -- Normalization condition: if one of the thresholds is zero, then there are no subfunctions.
  thresholdFunctions :: MultiSet.MultiSet f
  -- Arvid's comment: Right now, all the elements of thresholdFunctions must have the same
  -- type, i.e. Symmetric or ThresholdFun etc. Doesn't this limit what we can express?
} deriving (Show)

-- Necessitated by misdesign of Haskell typeclasses.
instance Eq1 ThresholdFun where
  liftEq eq' (ThresholdFun t us) (ThresholdFun t' us') =
    liftEq2 (==) (liftEq eq') (t, us) (t', us')

instance Ord1 ThresholdFun where
  liftCompare compare' (ThresholdFun t us) (ThresholdFun t' us') =
    liftCompare2 compare (liftCompare compare') (t, us) (t', us')

-- TODO: use record fields.
instance Show1 ThresholdFun where
  liftShowsPrec showsPrec' showList' p (ThresholdFun t u) =
    showsBinaryWith showsPrec (liftShowsPrec showsPrec' showList') "ThresholdFun" p t u

-- TODO: instance Read1 ThresholdFun

-- We iterate over the number of subfunctions
instance (Enumerable g, Ord g) => Enumerable (ThresholdFun g) where
  enumerate :: (Typeable f, Sized f) => Shared f (ThresholdFun g)
  -- enumerate = datatype [c2 ThresholdFun] -- This does not work since it generates illegal combinations of threshold and subfuns.
  enumerate = share $ go 0
    where
      go n = enumerateThresholdFun n <|> go (n + 1)

enumerateThresholdFun :: (Typeable f, Sized f, Ord g, Enumerable g) => Int -> Shareable f (ThresholdFun g)
enumerateThresholdFun n = (ThresholdFun . Threshold <$> tupleF) <*> multisetF
  where
    tupleF = enumerateTuples n
    multisetF = enumerateMultiSet n

-- Tuples are free
enumerateTuples :: (Typeable f, Sized f) => Int -> Shareable f (Int, Int)
enumerateTuples n = aconcat $ map pure $ [(nt, n + 1 - nt) | nt <- [0 .. n + 1]]

-- Enumerates MultiSets with exactly n members
-- MultSets are free
enumerateMultiSet :: (Typeable f, Sized f, Ord a, Enumerable a) => Int -> Shareable f (MultiSet a)
enumerateMultiSet 0 = pure MultiSet.empty
enumerateMultiSet n = insertMultiSetF access $ enumerateMultiSet (n - 1)

insertMultiSetF :: (Applicative f, Ord a) => f a -> f (MultiSet a) -> f (MultiSet a)
insertMultiSetF = liftA2 MultiSet.insert

instance Enumerable (Iterated ThresholdFun) where
  enumerate :: (Typeable f, Sized f) => Shared f (Iterated ThresholdFun)
  enumerate = datatype [
    c1 Pure,
    c1 Free]

{-enumerateITF :: Sized f => f (Iterated ThresholdFun)
enumerateITF = go 0 where
  go n = aconcat (map pure (allNaryITFs n)) <|> pay (go (n + 1))-}

-- TODO: Special case of transport of a type class along an isomorphism.
instance (Ord x, Memoizable x) => Memoizable (ThresholdFun x) where
  memoize :: (ThresholdFun x -> v) -> ThresholdFun x -> v
  memoize f = m >>> memoize (n >>> f) where
    -- Back and forth.
    m (ThresholdFun t us) = (t, us)
    n (t, us) = ThresholdFun t us

thresholdFunConst :: Bool -> ThresholdFun f
thresholdFunConst val = ThresholdFun (thresholdConst val) MultiSet.empty

-- | Normalizes threshold functions equivalent to a constant function.
thresholdFunNormalize :: ThresholdFun f -> ThresholdFun f
thresholdFunNormalize u = case thresholdIsConst (threshold u) of
  Just val -> thresholdFunConst val
  Nothing  -> u

-- Reduces constant subfunctions in a threshold function.
-- Not used for anything right now.
{-
thresholdFunNormalizeSub :: (Eq f, BoFun f i) => ThresholdFun f -> ThresholdFun f
thresholdFunNormalizeSub (ThresholdFun t us) = ThresholdFun (t - s) (MultiSet.fromAscOccurList us') where
  (reduced, us') = us
    & MultiSet.toOccurList
    & map (first viewConst >>> (\(x, k) -> bimap (, k) (, k) x))
    & partitionEithers
  s = reduced
    & map (\(val, k) -> thresholdScale k (thresholdConst val))
    & sum
-}

-- TODO: Figure out why this needs UndecidableInstances. (Haskell...)
instance (Ord f, BoFun f i) => BoFun (ThresholdFun f) (Int, i) where
  isConst :: ThresholdFun f -> Maybe Bool
  isConst = threshold >>> thresholdIsConst

  variables :: ThresholdFun f -> [(Int, i)]
  variables (ThresholdFun _ us) = do
    (i, (u, _)) <- us & MultiSet.toAscOccurList & zip naturals
    v <- variables u
    return (i, v)

  setBit :: ((Int, i), Bool) -> ThresholdFun f -> ThresholdFun f
  setBit ((i, v), val) (ThresholdFun t us) = case isConst u' of
    Just _  -> thresholdFunNormalize $ ThresholdFun t' us'
    Nothing -> ThresholdFun t $ MultiSet.insert u' us'
    where
    (u, _) = MultiSet.toAscOccurList us !! i
    us' = us & MultiSet.delete u
    u' = setBit (v, val) u
    t' = t - thresholdConst (not val)

instance Constable ThresholdFun where
  mkConst :: Bool -> ThresholdFun f
  mkConst = thresholdFunConst

-- | A thresholding function with copies of a single subfunction.
thresholdFunReplicate :: (Ord f) => Threshold -> f -> ThresholdFun f
thresholdFunReplicate t u = ThresholdFun t $ MultiSet.fromOccurList [(u, thresholdNumInputs t)]

-- | Boolean functions built from iterated thresholding.
type IteratedThresholdFun = Iterated ThresholdFun

arityIteratedThreshold :: Iterated ThresholdFun -> Int
arityIteratedThreshold = length . variables

iteratedThresholdFunConst :: Bool -> Iterated' ThresholdFun f
iteratedThresholdFunConst = thresholdFunConst >>> Free

-------------- Examples ------------------------------------

majFun :: Int -> ThresholdFun (Maybe Bool)
majFun n = thresholdFunReplicate (thresholdMaj n') Nothing
  where
    n' = (n `div` 2) + 1

iteratedFun :: [Threshold] -> IteratedThresholdFun
iteratedFun []       = Pure ()
iteratedFun (t : ts) = Free $ thresholdFunReplicate t $ iteratedFun ts

iteratedMajFun' :: [Int] -> IteratedThresholdFun
iteratedMajFun' = map thresholdMaj >>> iteratedFun

-- Argument are votes needed at each stage and number of stages.
iteratedMajFun :: Int -> Int -> IteratedThresholdFun
iteratedMajFun nBits numStages = replicate numStages threshold & iteratedMajFun'
  where
    threshold = (nBits + 1) `div` 2

instance Arbitrary (Iterated ThresholdFun) where
  arbitrary :: Gen (Iterated ThresholdFun)
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    generateIteratedThresholdFun n'

-- Generator equivalent of allNAryITFs
generateIteratedThresholdFun :: Int -> Gen (Iterated ThresholdFun)
generateIteratedThresholdFun 0 = elements
  [iteratedThresholdFunConst False, iteratedThresholdFunConst True]
generateIteratedThresholdFun 1 = elements
  [iteratedThresholdFunConst False, iteratedThresholdFunConst True, Pure ()]
generateIteratedThresholdFun n = do
  (subFuns, nSubFuns) <- generateSubFuns n
  threshold' <- generateThreshold nSubFuns
  return $ Free $ ThresholdFun threshold' subFuns

generateThreshold :: Int -> Gen Threshold
generateThreshold n = do
  nt <- chooseInt (0, n + 1)
  let nf = n + 1 - nt
  return $ Threshold (nt, nf)

generateSubFuns :: Int -> Gen (MultiSet (Free ThresholdFun ()), Int)
generateSubFuns n = do
  partition <- elements $ partitions n
  subFuns <- mapM generateIteratedThresholdFun partition
  return (MultiSet.fromList subFuns, length subFuns)

--------------- Exhaustive generation ------------------------------

-- Gives all possible representations of n-bit ITFs, except for the ones with
-- 0-ary functions as their subfunctions, as this would lead to an infinite
-- number of representations.
allNAryITFs :: Int -> [Iterated ThresholdFun]
allNAryITFs = (map nAryITFEnum' [0 ..] !!)
  where
    nAryITFEnum' 0 =
      [iteratedThresholdFunConst False, iteratedThresholdFunConst True]
    nAryITFEnum' 1 =
      [iteratedThresholdFunConst False, iteratedThresholdFunConst True, Pure ()]
    nAryITFEnum' n = do
      (subFuns, nSubFuns) <- allSubFunCombinations n
      threshold' <- allThresholds nSubFuns
      return $ Free $ ThresholdFun threshold' subFuns

-- Gives all the thresholds satisfying the following properties:
-- 0 <= tn <= n + 1
-- tn + tf = n + 1
allThresholds :: Int -> [Threshold]
allThresholds n = do
  nt <- [0 .. n + 1]
  let nf = n + 1 - nt
  return $ Threshold (nt, nf)

-- Generates all possible partitions of positive integers that add up to n.
-- The member elements of these partions represent the arities of the subfunctions.
-- For each arity, we then generate all possible subFunctions.
allSubFunCombinations :: Int -> [(MultiSet (Free ThresholdFun ()), Int)]
allSubFunCombinations n = do
  partition <- partitions n
  subFuns <- mapM allNAryITFs partition
  return (MultiSet.fromList subFuns, length subFuns)


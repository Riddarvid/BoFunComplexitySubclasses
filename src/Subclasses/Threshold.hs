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
  ThresholdFun,
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
import           Control.Applicative   ((<|>))
import           Control.Enumerable    (Shareable, Shared, Sized (aconcat, pay),
                                        share)
import           Data.MultiSet         (MultiSet)
import           Subclasses.Iterated   (Iterated, Iterated')
import           Subclasses.Lifted     (Lifted, LiftedSymmetric, mkLiftedSymm)
import           Test.Feat             (Enumerable (enumerate))
import           Test.QuickCheck       (Arbitrary (arbitrary), chooseInt,
                                        elements, sized)
import           Test.QuickCheck.Gen   (Gen)
import           Type.Reflection       (Typeable)
import           Utils                 (Square, chooseMany, duplicate,
                                        enumerateMultiSet, lookupBool, naturals,
                                        partitions, squareToList, tabulateBool)

-- | A threshold for a Boolean function.
-- Number of inputs needed for 'True' and 'False' result, respectively.
-- The sum of these thresholds equals the number of inputs plus one.
-- Each threshold is non-negative.
-- TODO-NEW: Normalized version
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

reduceThreshold :: Bool -> Threshold -> Threshold
reduceThreshold v (Threshold (nt, nf)) = if v
  then Threshold (nt - 1, nf)
  else Threshold (nt, nf - 1)

newtype BasicThresholdFun = BTF Threshold
  deriving (Eq, Ord, Show)

instance BoFun BasicThresholdFun () where
  isConst :: BasicThresholdFun -> Maybe Bool
  isConst (BTF th) = thresholdIsConst th
  variables :: BasicThresholdFun -> [()]
  variables f = case isConst f of
    Just _  -> []
    Nothing -> [()]
  setBit :: ((), Bool) -> BasicThresholdFun -> BasicThresholdFun
  setBit (_, v) (BTF th) = BTF $ reduceThreshold v th

type ThresholdFun = LiftedSymmetric BasicThresholdFun

thresholdFunConst :: Bool -> ThresholdFun f
thresholdFunConst val = mkLiftedSymm (BTF $ thresholdConst val) MultiSet.empty

instance Constable ThresholdFun where
  mkConst :: Bool -> ThresholdFun f
  mkConst = thresholdFunConst

-- | A thresholding function with copies of a single subfunction.
thresholdFunReplicate :: (Ord f) => Threshold -> f -> ThresholdFun f
thresholdFunReplicate t u = mkLiftedSymm (BTF t) $ MultiSet.fromOccurList [(u, thresholdNumInputs t)]

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

-------------- Generation of ITFs ---------------------------------------

instance Arbitrary (Iterated ThresholdFun) where
  arbitrary :: Gen (Iterated ThresholdFun)
  arbitrary = sized $ \n -> do
    n' <- chooseInt (0, n)
    generateNAryITF n'

-- Generator equivalent of allNAryITFs
generateNAryITF :: Int -> Gen (Iterated ThresholdFun)
generateNAryITF 0 = elements
  [iteratedThresholdFunConst False, iteratedThresholdFunConst True]
generateNAryITF 1 = elements
  [iteratedThresholdFunConst False, iteratedThresholdFunConst True, Pure ()]
generateNAryITF n = do
  (subFuns, nSubFuns) <- generateSubFuns n
  threshold' <- generateThreshold nSubFuns
  return $ Free $ mkLiftedSymm (BTF threshold') subFuns

generateThreshold :: Int -> Gen Threshold
generateThreshold n = do
  nt <- chooseInt (0, n + 1)
  let nf = n + 1 - nt
  return $ Threshold (nt, nf)

generateSubFuns :: Int -> Gen (MultiSet (Free ThresholdFun ()), Int)
generateSubFuns n = do
  partition <- elements $ partitions n
  subFuns <- mapM generateNAryITF partition
  return (MultiSet.fromList subFuns, length subFuns)

--------------- Enumeration -----------------------------

-- We iterate over the number of subfunctions
instance (Enumerable g, Ord g) => Enumerable (ThresholdFun g) where
  enumerate :: (Typeable f, Sized f) => Shared f (ThresholdFun g)
  -- enumerate = datatype [c2 ThresholdFun] -- This does not work since it generates illegal combinations of threshold and subfuns.
  enumerate = share $ go 0
    where
      go nSubFuns = pay $ enumerateThresholdFun nSubFuns <|> go (nSubFuns + 1)

-- TODO-NEW: Tydligare namn
enumerateThresholdFun :: (Typeable f, Sized f, Ord g, Enumerable g) => Int -> Shareable f (ThresholdFun g)
enumerateThresholdFun nSubFuns = mkLiftedSymm <$> tupleF <*> multisetF
  where
    tupleF = BTF <$> enumerateThresholds nSubFuns
    multisetF = enumerateMultiSet nSubFuns

-- Tuples are free
enumerateThresholds :: (Typeable f, Sized f) => Int -> Shareable f Threshold
enumerateThresholds nSubFuns = aconcat $ [pure $ Threshold (nt, nSubFuns + 1 - nt) | nt <- [0 .. nSubFuns + 1]]

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
      return $ Free $ mkLiftedSymm (BTF threshold') subFuns

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


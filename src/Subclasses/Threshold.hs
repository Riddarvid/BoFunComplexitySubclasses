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
  ThresholdFun,
  LiftedThresholdFun,
  majFun,
  iteratedMajFun,
  allNAryITFs
) where

import           Control.Monad.Free    (Free (..))
import           Data.Function.Memoize (deriveMemoizable)
import qualified Data.MultiSet         as MultiSet
import           Prelude               hiding (negate, sum, (+), (-))

import           DSLsofMath.Algebra    (Additive (..), (-))

import           ArbitraryArity        (ArbitraryArity (arbitraryArity))
import           BoFun                 (BoFun (..), Constable (mkConst))
import           Control.Applicative   ((<|>))
import           Control.Enumerable    (Shareable, Shared, Sized (aconcat, pay),
                                        share)
import           Data.MultiSet         (MultiSet)
import           Subclasses.Iterated   (IteratedSymm, iterateSymmFun, liftIter)
import           Subclasses.Lifted     (LiftedSymmetric, liftFunSymm)
import           Test.Feat             (Enumerable (enumerate))
import           Test.QuickCheck       (chooseInt)
import           Test.QuickCheck.Gen   (Gen)
import           Type.Reflection       (Typeable)
import           Utils                 (Square, enumerateMultiSet, partitions)

--------------- Threshold ----------------------------------------

-- | A threshold for a Boolean function.
-- Number of inputs needed for 'True' and 'False' result, respectively.
-- The sum of these thresholds equals the number of inputs plus one.
-- Each threshold is non-negative.
-- TODO-NEW: Normalized version, that is, only (1,0) and (0,1) are allowed as const funs.
newtype Threshold = Threshold (Square Int)
  deriving (Show, Eq, Ord)

$(deriveMemoizable ''Threshold)

thresholdArity :: Threshold -> Int
thresholdArity (Threshold (nt, nf)) = nt + nf - 1

-- A constant threshold (fixed result).
thresholdConst :: Bool -> Threshold
thresholdConst False = Threshold (1, 0)
thresholdConst True  = Threshold (0, 1)

thresholdIsConst :: Threshold -> Maybe Bool
thresholdIsConst (Threshold (nt, nf)) = if
  | nt <= 0   -> Just True
  | nf <= 0   -> Just False
  | otherwise -> Nothing

reduceThreshold :: Bool -> Threshold -> Threshold
reduceThreshold v (Threshold (nt, nf)) = if v
  then Threshold (nt - 1, nf)
  else Threshold (nt, nf - 1)

---------------------- Threshold function ----------------------------

newtype ThresholdFun = ThresholdFun Threshold
  deriving (Eq, Ord, Show)

$(deriveMemoizable ''ThresholdFun)

instance BoFun ThresholdFun () where
  isConst :: ThresholdFun -> Maybe Bool
  isConst (ThresholdFun th) = thresholdIsConst th
  variables :: ThresholdFun -> [()]
  variables f = case isConst f of
    Just _  -> []
    Nothing -> [()]
  setBit :: ((), Bool) -> ThresholdFun -> ThresholdFun
  setBit (_, v) (ThresholdFun th) = ThresholdFun $ reduceThreshold v th

instance Constable ThresholdFun where
  mkConst :: Bool -> ThresholdFun
  mkConst = ThresholdFun . thresholdConst

thresholdFunArity :: ThresholdFun -> Int
thresholdFunArity (ThresholdFun t) = thresholdArity t

----------------- Lifted Threshold Function ------------------

type LiftedThresholdFun = LiftedSymmetric ThresholdFun

-- | A thresholding function with copies of a single subfunction.
thresholdFunReplicate :: (Ord f) => ThresholdFun -> f -> LiftedThresholdFun f
thresholdFunReplicate t u = liftFunSymm t $ MultiSet.fromOccurList [(u, thresholdFunArity t)]

-------------- Examples ------------------------------------

majFun :: Int -> ThresholdFun
majFun bits = ThresholdFun $ Threshold (n, n)
  where
    n = (bits `div` 2) + 1

iteratedMajFun :: Int -> Int -> IteratedSymm ThresholdFun
iteratedMajFun bits = iterateSymmFun bits (majFun bits)

-------------- Generation of ITFs ---------------------------------------

-- TODO-NEW: These instances can probably be generalized and moved to Iterated
instance ArbitraryArity ThresholdFun where
  arbitraryArity :: Int -> Gen ThresholdFun
  arbitraryArity arity = ThresholdFun <$> generateThreshold arity

generateThreshold :: Int -> Gen Threshold
generateThreshold arity = do
  nt <- chooseInt (0, arity + 1)
  let nf = arity + 1 - nt
  return $ Threshold (nt, nf)

--------------- Enumeration -----------------------------

enumerateNAry :: (Typeable f, Sized f) =>Int -> Shareable f ThresholdFun
enumerateNAry arity = aconcat $
  [pure $ ThresholdFun $ Threshold (nt, arity + 1 - nt) | nt <- [0 .. arity + 1]]

-- We iterate over the number of subfunctions
instance (Enumerable g, Ord g) => Enumerable (LiftedThresholdFun g) where
  enumerate :: (Typeable f, Sized f) => Shared f (LiftedThresholdFun g)
  -- enumerate = datatype [c2 LiftedThresholdFun] -- This does not work since it generates illegal combinations of threshold and subfuns.
  enumerate = share $ go 0
    where
      go nSubFuns = pay $ enumerateThresholdFun nSubFuns <|> go (nSubFuns + 1)

-- TODO-NEW: Tydligare namn
enumerateThresholdFun :: (Typeable f, Sized f, Ord g, Enumerable g) => Int -> Shareable f (LiftedThresholdFun g)
enumerateThresholdFun nSubFuns = liftFunSymm <$> tupleF <*> multisetF
  where
    tupleF = ThresholdFun <$> enumerateThresholds nSubFuns
    multisetF = enumerateMultiSet nSubFuns

-- Tuples are free
enumerateThresholds :: (Typeable f, Sized f) => Int -> Shareable f Threshold
enumerateThresholds nSubFuns = aconcat $ [pure $ Threshold (nt, nSubFuns + 1 - nt) | nt <- [0 .. nSubFuns + 1]]

--------------- Exhaustive generation ------------------------------

-- Gives all possible representations of n-bit ITFs, except for the ones with
-- 0-ary functions as their subfunctions, as this would lead to an infinite
-- number of representations.
allNAryITFs :: Int -> [IteratedSymm ThresholdFun]
allNAryITFs = (map nAryITFEnum' [0 ..] !!)
  where
    nAryITFEnum' 0 =
      [mkConst False, mkConst True]
    nAryITFEnum' 1 =
      [mkConst False, mkConst True, Pure ()]
    nAryITFEnum' n = do
      (subFuns, nSubFuns) <- allSubFunCombinations n
      threshold' <- allThresholds nSubFuns
      return $ liftIter $ liftFunSymm (ThresholdFun threshold') subFuns

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
allSubFunCombinations :: Int -> [(MultiSet (IteratedSymm ThresholdFun), Int)]
allSubFunCombinations n = do
  partition <- partitions n
  subFuns <- mapM allNAryITFs partition
  return (MultiSet.fromList subFuns, length subFuns)


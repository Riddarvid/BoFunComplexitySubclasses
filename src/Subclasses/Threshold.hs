{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
module Subclasses.Threshold (
  Threshold(Threshold),
  ThresholdFun(ThresholdFun),
  NonSymmThresholdFun(NonSymmThresholdFun),
  majFun,
  iteratedMajFun,
  allNAryITFs
) where

import           Data.Function.Memoize        (Memoizable, deriveMemoizable)
import           Prelude                      hiding (negate, sum, (+), (-))

import           DSLsofMath.Algebra           (Additive (..), (-))

import           Arity                        (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun             (BoFun (..), Constable (mkConst))
import           Control.DeepSeq              (NFData)
import           Exploration.PrettyPrinting   (PrettyBoFun (prettyShow))
import           GHC.Generics                 (Generic)
import           Subclasses.Iterated.Iterated (Iterated,
                                               Iterated' (Id, Iterated),
                                               iterateFun)
import           Subclasses.Lifted            (Lifted (Lifted))
import           Test.QuickCheck              (chooseInt)
import           Test.QuickCheck.Gen          (Gen)
import           Utils                        (Square, naturals, partitions)

--------------- Threshold ----------------------------------------

-- | A threshold for a Boolean function.
-- Number of inputs needed for 'True' and 'False' result, respectively.
-- The sum of these thresholds equals the number of inputs plus one.
-- Each threshold is non-negative.
newtype Threshold = Threshold (Square Int)
  deriving (Show, Eq, Ord, Generic)

instance NFData Threshold

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
  deriving (Eq, Ord, Generic, Show)

$(deriveMemoizable ''ThresholdFun)

instance PrettyBoFun ThresholdFun where
  prettyShow :: ThresholdFun -> String
  prettyShow f@(ThresholdFun (Threshold (nt, _))) = show arity ++ "-bit threshold function, threshold at " ++ show nt
    where
      arity = thresholdFunArity f

instance NFData ThresholdFun

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

newtype NonSymmThresholdFun = ThresholdFun' ThresholdFun
  deriving (Memoizable, NFData, Show)

pattern NonSymmThresholdFun :: Threshold -> NonSymmThresholdFun
pattern NonSymmThresholdFun th = ThresholdFun' (ThresholdFun th)

instance PrettyBoFun NonSymmThresholdFun where
  prettyShow :: NonSymmThresholdFun -> String
  prettyShow (ThresholdFun' f) = prettyShow f

instance BoFun NonSymmThresholdFun Int where
  isConst :: NonSymmThresholdFun -> Maybe Bool
  isConst (ThresholdFun' f) = isConst f
  variables :: NonSymmThresholdFun -> [Int]
  variables (ThresholdFun' f) = take (thresholdFunArity f) naturals
  setBit :: (Int, Bool) -> NonSymmThresholdFun -> NonSymmThresholdFun
  setBit (_, v) (ThresholdFun' f) = ThresholdFun' $ setBit ((), v) f

----------------- Lifted Threshold Function ------------------

-- | A thresholding function with copies of a single subfunction.
thresholdFunReplicate :: (BoFun f i) => ThresholdFun -> f -> Lifted NonSymmThresholdFun f
thresholdFunReplicate f g = Lifted (ThresholdFun' f) $ replicate (thresholdFunArity f) g

-------------- Examples ------------------------------------

majFun :: Int -> ThresholdFun
majFun bits = ThresholdFun $ Threshold (n, n)
  where
    n = (bits `div` 2) + 1

iteratedMajFun :: Int -> Int -> Iterated NonSymmThresholdFun
iteratedMajFun bits = iterateFun bits (ThresholdFun' $ majFun bits)

-------------- Generation of ITFs ---------------------------------------

instance ArbitraryArity ThresholdFun where
  arbitraryArity :: Int -> Gen ThresholdFun
  arbitraryArity arity = ThresholdFun <$> generateThreshold arity

generateThreshold :: Int -> Gen Threshold
generateThreshold arity = do
  nt <- chooseInt (0, arity + 1)
  let nf = arity + 1 - nt
  return $ Threshold (nt, nf)

--------------- Enumeration -----------------------------
-- Is not really used for much right now.

-- enumerateNAry :: (Typeable f, Sized f) =>Int -> Shareable f ThresholdFun
-- enumerateNAry arity = aconcat $
--   [pure $ ThresholdFun $ Threshold (nt, arity + 1 - nt) | nt <- [0 .. arity + 1]]

-- -- We iterate over the number of subfunctions
-- instance (Enumerable g, Ord g) => Enumerable (LiftedThresholdFun g) where
--   enumerate :: (Typeable f, Sized f) => Shared f (LiftedThresholdFun g)
--   -- enumerate = datatype [c2 LiftedThresholdFun] -- This does not work since it generates illegal combinations of threshold and subfuns.
--   enumerate = share $ go 0
--     where
--       go nSubFuns = pay $ enumerateThresholdFun nSubFuns <|> go (nSubFuns + 1)

-- enumerateThresholdFun :: (Typeable f, Sized f, Ord g, Enumerable g) => Int -> Shareable f (LiftedThresholdFun g)
-- enumerateThresholdFun nSubFuns = toLifted <$> tupleF <*> multisetF
--   where
--     tupleF = ThresholdFun <$> enumerateThresholds nSubFuns
--     multisetF = enumerateMultiSet nSubFuns

-- -- Tuples are free
-- enumerateThresholds :: (Typeable f, Sized f) => Int -> Shareable f Threshold
-- enumerateThresholds nSubFuns = aconcat $ [pure $ Threshold (nt, nSubFuns + 1 - nt) | nt <- [0 .. nSubFuns + 1]]

--------------- Exhaustive generation ------------------------------

-- Gives all possible representations of n-bit ITFs, except for the ones with
-- 0-ary functions as their subfunctions, as this would lead to an infinite
-- number of representations.
allNAryITFs :: Int -> [Iterated NonSymmThresholdFun]
allNAryITFs = (map nAryITFEnum' [0 ..] !!)
  where
    nAryITFEnum' 0 =
      [mkConst False, mkConst True]
    nAryITFEnum' 1 =
      [mkConst False, mkConst True, Id]
    nAryITFEnum' n = do
      (subFuns, nSubFuns) <- allSubFunCombinations n
      threshold' <- allThresholds nSubFuns
      return $ Iterated (NonSymmThresholdFun threshold') subFuns

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
allSubFunCombinations :: Int -> [([Iterated NonSymmThresholdFun], Int)]
allSubFunCombinations n = do
  partition <- partitions n
  subFuns <- mapM allNAryITFs partition
  return (subFuns, length subFuns)


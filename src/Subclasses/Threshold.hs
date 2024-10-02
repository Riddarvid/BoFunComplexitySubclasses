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
  iteratedThresholdFunConst
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
import           Subclasses.Iterated   (Iterated, Iterated')
import           Utils                 (Square, boolToInt, chooseMany,
                                        duplicate, lookupBool, naturals,
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

-- A constant threshold (fixed result).
-- TODO-NEW: Figure out how we want to handle this bugfix.
-- fixed by changing == to /=
-- we also had to change to thresholdConst (not val) in the definition of setBit.
thresholdConst :: Bool -> Threshold
thresholdConst v = Threshold $ tabulateBool ((== v) >>> boolToInt)

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

{-
TODO:
Figure out why this doesn't work.
The error message is:
    • ‘Threshold’ is not in the type environment at a reify
    • In the untyped splice: $(deriveMemoize ''ThresholdFun)
-}
-- instance (Ord x, Memoizable x) => Memoizable (ThresholdFun x) where
--   memoize = $(deriveMemoize ''ThresholdFun)

-- TODO: Special case of transport of a type class along an isomorphism.
instance (Ord x, Memoizable x) => Memoizable (ThresholdFun x) where
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

-- QUESTION: It seems like we start by having a BoFun f and then "promoting" it to a
-- ThresholdFun. It is a bit unclear what the variable type actually means.
-- How does setBit work here?

-- OUR UNDERSTANDING: A threshold function is made up of a list of sub-functions.
-- The variables of our function can then be described by a combination of sub-function
-- and sub-function-variable. The basic principle is that any time a sub function becomes
-- fully evaluated, we can reduce the appropriate threshold variable by one, and if the
-- threshold becomes zero, then we are done.

-- TODO: Figure out why this needs UndecidableInstances. (Haskell...)
instance (Ord f, BoFun f i) => BoFun (ThresholdFun f) (Int, i) where
  isConst = threshold >>> thresholdIsConst

  variables (ThresholdFun _ us) = do
    (i, (u, _)) <- us & MultiSet.toAscOccurList & zip naturals
    v <- variables u
    return (i, v)

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

iteratedThresholdFunConst :: Bool -> Iterated' ThresholdFun f
iteratedThresholdFunConst = thresholdFunConst >>> Free

{-
-- General instance.
-- Conflicts with the specialized instance for IteratedThresholdFun' given below.
instance (Ord f, BoFun f i) => BoFun (IteratedThresholdFun f) ([Int], i) where
  isConst (Pure u) = isConst u
  isConst (Free v) = isConst v

  variables (Pure u) = variables u & map ([],)
  variables (Free v) = variables v & map (\(i, (is, j)) -> (i : is, j))

  setBit (([], j), val) (Pure u) = Pure $ setBit (j, val) u
  setBit ((i : is, j), val) (Free v) = Free $ setBit ((i, (is, j)), val) v
-}
-- Pure () represents the id function.

-- Example Boolean functions.

-- | Majority on five bits
maj5 :: ThresholdFun (Maybe Bool)
maj5 = majFun 5

majFun :: Int -> ThresholdFun (Maybe Bool)
majFun n = thresholdFunReplicate (thresholdMaj n') Nothing
  where
    n' = (n `div` 2) + 1

iteratedFun :: [Threshold] -> IteratedThresholdFun
iteratedFun []       = Pure ()
iteratedFun (t : ts) = Free $ thresholdFunReplicate t $ iteratedFun ts

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

iteratedMajFun' :: [Int] -> IteratedThresholdFun
iteratedMajFun' = map thresholdMaj >>> iteratedFun

-- Argument are votes needed at each stage and number of stages.
iteratedMajFun :: Int -> Int -> IteratedThresholdFun
iteratedMajFun nBits numStages = replicate numStages threshold & iteratedMajFun'
  where
    threshold = (nBits + 1) `div` 2

iteratedMaj3 :: Int -> IteratedThresholdFun
iteratedMaj3 = iteratedMajFun 3
{-
The number of Boolean functions reachable from iteratedMaj3 is 2 plus s_n where
* s_0 = 1,
* s_{n+1} = s_n (s_n + 2) (s_n + 7) / 6.
For example:
* s_0 = 1,
* s_1 = 4
* s_2 = 44,
* s_3 = 17204,
* s_4 = 849110490844,
-}

iteratedMaj5 :: Int -> IteratedThresholdFun
iteratedMaj5 = iteratedMajFun 5
{-
The number of Boolean functions reachable from iteratedMaj5 is 2 plus t_n where
* t_0 = 1,
* t_{n+1} = t_n (t_n + 2) (t_n + 3) (t_n ^ 2 + 15 t_n + 74) / 120
For example:
* t_0 = 1,
* t_1 = 9,
* t_2 = 2871,
* t_3 = 1636845671105073,
* t_4 = 97916848002123806402045274379974531999764335775612939415896877758995991565.
-}



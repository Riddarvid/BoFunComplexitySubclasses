{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module BDD.BDDInstances () where
import           BDD                       (BDDFun, bddFromOutput, normalizeBDD)
import           BoFun                     (BoFun (..))
import           Data.DecisionDiagram.BDD  (BDD (..), ItemOrder, Sig, false,
                                            inSig, outSig, restrict, support,
                                            true)
import           Data.Function.Memoize     (Memoizable (memoize),
                                            deriveMemoizable)
import qualified Data.IntSet               as IS
import           Test.QuickCheck           (Arbitrary (arbitrary), Gen,
                                            chooseInt, resize, sized, vector)
import           Test.QuickCheck.Arbitrary (shrink)

$(deriveMemoizable ''Sig)

instance Memoizable (BDD o) where
  memoize :: (BDD o -> v) -> BDD o -> v
  memoize = memoizeBF

memoizeBF :: (BDD o -> a) -> (BDD o -> a)
memoizeBF f = memoize (f . inSig) . outSig

-- For some reason the following functions do not work.
-- TODO-NEW: Figure out why
{-
memoizeBF'' :: (BDD o -> a) -> (BDD o -> a)
memoizeBF'' f bdd = memoize (\sig -> f $ inSig sig) $ outSig bdd

memoizeBF' :: (BDD o -> a) -> (BDD o -> a)
memoizeBF' f = \bdd -> case bdd of
  Leaf v             -> memoize (f . Leaf) v
  Branch n bdd1 bdd2 -> memoize (\(n', bdd1', bdd2') -> f $ Branch n' bdd1' bdd2') (n, bdd1, bdd2)
-}

instance BoFun BDDFun Int where
  isConst :: BDDFun -> Maybe Bool
  isConst = isConstBDD
  variables :: BDDFun -> [Int]
  variables = IS.toList . support
  setBit :: (Int, Bool) -> BDDFun -> BDDFun
  setBit (i, v) = normalizeBDD . restrict i v

isConstBDD :: BDD o -> Maybe Bool
isConstBDD (Leaf b) = Just b
isConstBDD _        = Nothing

-- TODO-NEW: The resizing really shouldn't be done here.
instance Arbitrary BDDFun where
  arbitrary :: Gen BDDFun
  arbitrary = resize 4 $ sized genFun

  shrink :: BDDFun -> [BDDFun]
  shrink bdd
    | bdd /= bdd' = shrink bdd'
    where
      bdd' = normalizeBDD bdd
  shrink bdd = case bdd of
    Leaf v    -> if v then [false] else []
    Branch {} -> [false, true] ++ [normalizeBDD $ restrict v val bdd | v <- vars, val <- [False, True]]
    where
      vars = IS.toAscList $ support bdd

genFun :: ItemOrder o => Int -> Gen (BDD o)
genFun n' = do
  n <- chooseInt (0, n')
  output <- vector (2^n)
  return $ bddFromOutput n output

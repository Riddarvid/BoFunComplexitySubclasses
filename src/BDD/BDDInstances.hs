{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
module BDD.BDDInstances () where
import           BDD                      (bddFromOutput)
import           BoFun                    (BoFun (..))
import           Data.DecisionDiagram.BDD (BDD (..), ItemOrder, Sig, inSig,
                                           outSig, restrict, support)
import           Data.Function.Memoize    (Memoizable (memoize),
                                           deriveMemoizable)
import qualified Data.IntSet              as IS
import           Test.QuickCheck          (Arbitrary (arbitrary), Gen,
                                           chooseInt, resize, sized, vector)

$(deriveMemoizable ''Sig)

instance Memoizable (BDD o) where
  memoize :: (BDD o -> v) -> BDD o -> v
  memoize = memoizeBF

memoizeBF :: (BDD o -> a) -> (BDD o -> a)
memoizeBF f = memoize (f . inSig) . outSig

instance ItemOrder o => BoFun (BDD o) Int where
  isConst :: BDD o -> Maybe Bool
  isConst = isConstBDD
  variables :: BDD o -> [Int]
  variables = IS.toList . support
  setBit :: (Int, Bool) -> BDD o -> BDD o
  setBit (i, v) = restrict i v

isConstBDD :: BDD o -> Maybe Bool
isConstBDD (Leaf b) = Just b
isConstBDD _        = Nothing

instance ItemOrder o => Arbitrary (BDD o) where
  arbitrary :: Gen (BDD o)
  arbitrary = resize 5 $ sized genFun

genFun :: ItemOrder o => Int -> Gen (BDD o)
genFun n' = do
  n <- chooseInt (0, n')
  output <- vector (2^n)
  return $ bddFromOutput n output

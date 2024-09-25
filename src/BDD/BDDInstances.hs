{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module BDD.BDDInstances () where
import           BoFun                    (BoFun (..))
import           Data.DecisionDiagram.BDD (BDD (..), ItemOrder, Sig, inSig,
                                           outSig, restrict, support)
import           Data.Function.Memoize    (Memoizable (memoize),
                                           deriveMemoizable)
import qualified Data.IntSet              as IS

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

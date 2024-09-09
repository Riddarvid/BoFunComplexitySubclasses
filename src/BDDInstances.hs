{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module BDDInstances () where
import           BoFun                    (BoFun (..))
import           Data.DecisionDiagram.BDD (BDD (..), ItemOrder, Sig, inSig,
                                           outSig, restrict, support)
import           Data.Function.Memoize    (Memoizable (memoize),
                                           deriveMemoizable)
import qualified Data.IntSet              as IS

type BDDFun o = BDD o

$(deriveMemoizable ''Sig)

instance Memoizable (BDDFun o) where
  memoize :: (BDDFun o -> v) -> BDDFun o -> v
  memoize = memoizeBF

memoizeBF :: (BDDFun o -> a) -> (BDDFun o -> a)
memoizeBF f = memoize (f . inSig) . outSig

instance ItemOrder a => BoFun (BDDFun a) Int where
  isConst :: BDDFun a -> Maybe Bool
  isConst = isConstBDD
  variables :: BDDFun a -> [Int]
  variables = IS.toList . support
  setBit :: (Int, Bool) -> BDDFun a -> BDDFun a
  setBit (i, v) = restrict i v

isConstBDD :: BDD a -> Maybe Bool
isConstBDD (Leaf b) = Just b
isConstBDD _        = Nothing

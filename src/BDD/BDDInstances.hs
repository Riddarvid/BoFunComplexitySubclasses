{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
module BDD.BDDInstances () where

import           BDD.BDD                  (boolToBDD, pick)
import           Complexity.Algor         (Algor (..))
import           Control.DeepSeq          (NFData (rnf))
import           Data.DecisionDiagram.BDD (BDD, ItemOrder, Sig, inSig, outSig)
import           Data.Function.Memoize    (Memoizable (memoize),
                                           deriveMemoizable)

$(deriveMemoizable ''Sig)

instance Memoizable (BDD o) where
  memoize :: (BDD o -> v) -> BDD o -> v
  memoize = memoizeBF

memoizeBF :: (BDD o -> a) -> (BDD o -> a)
memoizeBF f = memoize (f . inSig) . outSig

instance ItemOrder o => Algor (BDD o) where
  res :: Bool -> BDD o
  res = boolToBDD
  pic :: Int -> BDD o -> BDD o -> BDD o
  pic = pick

instance NFData a => NFData (Sig a)

instance NFData (BDD o) where
  rnf :: BDD o -> ()
  rnf = rnf . outSig

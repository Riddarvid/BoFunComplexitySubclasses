{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
module BDD.BDDInstances () where

import           Algorithm.Algor          (Algor (..))
import           BDD.BDD                  (pick)
import           Control.DeepSeq          (NFData (rnf))
import           Data.DecisionDiagram.BDD (BDD, ItemOrder, Sig, false, inSig,
                                           outSig, true)
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
  res False = false
  res True  = true
  pic :: Int -> BDD o -> BDD o -> BDD o
  pic = pick

instance NFData a => NFData (Sig a)

instance NFData (BDD o) where
  rnf :: BDD o -> ()
  rnf = rnf . outSig

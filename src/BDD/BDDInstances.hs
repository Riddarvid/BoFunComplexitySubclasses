{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE StandaloneDeriving    #-}
module BDD.BDDInstances () where

import           Algorithm.Algor          (Algor (..))
import           BDD                      (BDDFun, pick)
import           Data.DecisionDiagram.BDD (BDD, Sig, false, inSig, outSig, true)
import           Data.Function.Memoize    (Memoizable (memoize),
                                           deriveMemoizable)
import           Data.Ord                 (comparing)

-- Memoization

$(deriveMemoizable ''Sig)

instance Memoizable (BDD o) where
  memoize :: (BDD o -> v) -> BDD o -> v
  memoize = memoizeBF

memoizeBF :: (BDD o -> a) -> (BDD o -> a)
memoizeBF f = memoize (f . inSig) . outSig

instance Algor BDDFun where
  res :: Bool -> BDDFun
  res False = false
  res True  = true
  pic :: Int -> BDDFun -> BDDFun -> BDDFun
  pic = pick

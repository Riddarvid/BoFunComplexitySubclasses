{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module BDD (BDDFun) where
import           All                      (support)
import           BoFun                    (BoFun (..))
import           Data.DecisionDiagram.BDD (BDD (..), ItemOrder, ite, numNodes,
                                           substSet, var)
import qualified Data.IntSet              as IS

type BDDFun a = BDD a

pick :: ItemOrder a => Int -> BDDFun a -> BDDFun a -> BDDFun a
pick i a0 a1 = ite (var i) a1 a0

instance BoFun (BDDFun a) Int where
  isConst :: BDDFun a -> Maybe Bool
  isConst = isConstBDD
  variables :: BDDFun a -> [Int]
  variables = IS.toList . support
  setBit :: (Int, Bool) -> BDDFun a -> BDDFun a
  setBit (i, v) = restrict i v

isConstBDD :: BDD a -> Maybe Bool
isConstBDD (Leaf b) = Just b
isConstBDD _        = Nothing

{-

instance Ord (BDD a)  where
  compare :: BDD a -> BDD a -> Ordering
  compare = compareBDD

-- smaller BDDs sort first, then Leaf < Branch
compareBDD :: BDD a -> BDD a -> Ordering
compareBDD x y = thenCmp (compare (numNodes x) (numNodes y))
                         (compareBDD' x y)

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1

-- This uses the "smart patterns" defined in the BDD module
compareBDD' :: BDD a -> BDD a -> Ordering
compareBDD' (Leaf x) (Leaf y) = compare x y
compareBDD' (Leaf x) _        = LT
compareBDD' _        (Leaf y) = GT
compareBDD' (Branch li l0 l1) (Branch ri r0 r1) = compare (li, l0, l1) (ri, r0, r1) -}

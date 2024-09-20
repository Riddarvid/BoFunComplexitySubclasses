{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.Symmetric (
  BasicSymmetric,
  mkBasicSymmetric,
  Symmetric,
  symmResultvector,
  symmSubFuns,
  majSymm,
  majSymmBasic
) where

import           BoFun                 (BoFun (..))
import           Control.Arrow         ((>>>))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoizable)
import           Data.MultiSet         (MultiSet)
import qualified Data.MultiSet         as MultiSet
import           Utils                 (naturals)

--------- BasicSymmetric -----------------------------

newtype BasicSymmetric = BasicSymmetric [Bool]
  deriving Show

instance BoFun BasicSymmetric () where
  isConst :: BasicSymmetric -> Maybe Bool
  isConst (BasicSymmetric xs)
    | and xs = Just True
    | all not xs = Just False
    | otherwise = Nothing
  variables :: BasicSymmetric -> [()]
  variables (BasicSymmetric xs)
    | length xs <= 1 = []
    | otherwise = [()]
  setBit :: ((), Bool) -> BasicSymmetric -> BasicSymmetric
  setBit (_, v) (BasicSymmetric xs)
    | v = BasicSymmetric $ tail xs
    | otherwise = BasicSymmetric $ init xs

$(deriveMemoizable ''BasicSymmetric)

-- eval must be defined for [0 .. nBits]
mkBasicSymmetric :: Int -> (Int -> Bool) -> BasicSymmetric
mkBasicSymmetric nBits eval = BasicSymmetric $ map eval [0 .. nBits]

-- Only defined for positive, odd values of n.
majSymmBasic :: Int -> BasicSymmetric
majSymmBasic n = mkBasicSymmetric n (>= threshold)
  where
    threshold = (n `div` 2) + 1

---------------- Symmetric -----------------------------------------

data Symmetric f = Symmetric {
  symmResultvector :: [Bool],
  symmSubFuns      :: MultiSet f
}

instance (Ord f, Memoizable f) => Memoizable (Symmetric f) where
  memoize :: (Symmetric f -> a) -> Symmetric f -> a
  memoize f = m >>> memoize (n >>> f) where
    -- Back and forth.
    m (Symmetric rv subFuns) = (rv, subFuns)
    n (rv, subFuns) = Symmetric rv subFuns

instance (Ord f, BoFun f i) => BoFun (Symmetric f) (Int, i) where
  isConst :: Symmetric f -> Maybe Bool
  isConst f
    | and $ symmResultvector f = Just True
    | all not $ symmResultvector f = Just False
    | otherwise = Nothing
  variables :: Symmetric f -> [(Int, i)]
  variables (Symmetric _ subFuns) = do
    (i, (u, _)) <- subFuns & MultiSet.toAscOccurList & zip naturals
    v <- variables u
    return (i, v)
  setBit :: ((Int, i), Bool) -> Symmetric f -> Symmetric f
  setBit ((i, v), val) (Symmetric rv subFuns) = case isConst subFun' of
    Just res  -> Symmetric rv' subFuns'
      where rv' = if res then tail rv else init rv
    Nothing -> Symmetric rv $ MultiSet.insert subFun' subFuns'
    where
    (subFun, _) = MultiSet.toAscOccurList subFuns !! i
    subFuns' = subFuns & MultiSet.delete subFun
    subFun' = setBit (v, val) subFun

majSymm :: Int -> Symmetric (Maybe Bool)
majSymm n = Symmetric (replicate n' False ++ replicate n' True) $ MultiSet.fromOccurList [(Nothing, n)]
  where
    n' = (n `div` 2) + 1

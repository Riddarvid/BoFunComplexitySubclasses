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
  majSymmBasic,
  maj33,
  iteratedMajFun
) where

import           BoFun                 (BoFun (..))
import           Control.Arrow         ((>>>))
import           Control.Monad.Free    (Free (Free, Pure))
import           Data.Function         ((&))
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoizable,
                                        deriveMemoize)
import           Data.Functor.Classes  (Eq1 (liftEq), Eq2 (liftEq2),
                                        Ord1 (liftCompare), Ord2 (liftCompare2),
                                        Show1 (liftShowsPrec), showsBinaryWith)
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

instance Eq1 Symmetric where
  liftEq :: (a -> b -> Bool) -> Symmetric a -> Symmetric b -> Bool
  liftEq cmp (Symmetric res1 subs1) (Symmetric res2 subs2)
    = liftEq2 (==) (liftEq cmp) (res1, subs1) (res2, subs2)

instance Ord1 Symmetric where
  liftCompare :: (a -> b -> Ordering) -> Symmetric a -> Symmetric b -> Ordering
  liftCompare cmp (Symmetric res1 subs1) (Symmetric res2 subs2)
    = liftCompare2 compare (liftCompare cmp) (res1, subs1) (res2, subs2)

instance Show1 Symmetric where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Symmetric a -> ShowS
  liftShowsPrec showsPrec' showList' p (Symmetric res subFun) =
    showsBinaryWith showsPrec (liftShowsPrec showsPrec' showList') "SymmetricFun" p res subFun

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

constSymm :: Bool -> Symmetric a
constSymm val = Symmetric [val] MultiSet.empty

----------- Iterated Symmetric -------------------------

-- Implemented very similarly to IteratedThresholdFun.
type IteratedSymmetricFun' f = Free Symmetric f
type IteratedSymmetricFun = IteratedSymmetricFun' ()

instance (Ord f, Memoizable f) => Memoizable (IteratedSymmetricFun' f) where
  memoize :: (IteratedSymmetricFun' f -> v) -> IteratedSymmetricFun' f -> v
  memoize = $(deriveMemoize ''Free)

instance BoFun IteratedSymmetricFun [Int] where
  isConst :: IteratedSymmetricFun -> Maybe Bool
  isConst (Pure ()) = Nothing
  isConst (Free u)  = isConst u

  variables :: IteratedSymmetricFun -> [[Int]]
  variables (Pure ()) = [[]]
  variables (Free f)  = variables f & map (uncurry (:))

  setBit :: ([Int], Bool) -> IteratedSymmetricFun -> IteratedSymmetricFun
  setBit ([], val) (Pure _)     = Free $ constSymm val
  setBit (i : is, val) (Free f) = Free $ setBit ((i, is), val) f
  setBit _ _                    = error "Should not happen"

iteratedSymmFun :: [(Int, [Bool])] -> IteratedSymmetricFun
iteratedSymmFun [] = Pure ()
iteratedSymmFun ((nBits, res) : ress) = Free $
  Symmetric res $ MultiSet.fromOccurList [(subFun, nBits)]
  where
    subFun = iteratedSymmFun ress

iteratedMajFun :: Int -> Int -> IteratedSymmetricFun
iteratedMajFun nBits numStages = replicate numStages nBits & iteratedMajFun'

iteratedMajFun' :: [Int] -> IteratedSymmetricFun
iteratedMajFun' = iteratedSymmFun . map (\nBits -> (nBits, majRes nBits))

majRes :: Int -> [Bool]
majRes nBits = replicate votes False ++ replicate votes True
  where
    votes = (nBits + 1) `div` 2

maj33 :: IteratedSymmetricFun
maj33 = iteratedMajFun 3 2

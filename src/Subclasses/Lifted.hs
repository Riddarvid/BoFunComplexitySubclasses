{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Subclasses.Lifted (
  Lifted,
  mkLifted,
  LiftedSymmetric,
  mkLiftedSymm
) where
import           BoFun         (BoFun (..))
import           Data.Maybe    (fromJust)
import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import           Utils         (naturals)

-- Invariant: g only contains non-const functions
-- TODO-NEW: "smart constructor" ensuring this.
data Lifted f g = Lifted {
  lFun     :: f,
  lSubFuns :: [g]
}

-- TODO-NEW handle constant g's
mkLifted :: f -> [g] -> Lifted f g
mkLifted = Lifted

newtype NormalizedLifted f g = NL (Lifted f g)

instance (BoFun f Int, BoFun g j) => BoFun (Lifted f g) (Int, j) where
  isConst :: Lifted f g -> Maybe Bool
  isConst = isConst . lFun
  variables :: Lifted f g -> [(Int, j)]
  variables lf = do
    (subFun, i) <- zip (lSubFuns lf) naturals
    j <- variables subFun
    return (i, j)
  setBit :: ((Int, j), Bool) -> Lifted f g -> Lifted f g
  setBit ((i, j), v) lf = case isConst subFun' of
    Nothing -> lf{lSubFuns = start ++ (subFun : end)}
    Just v' -> lf{lFun = setBit (i, v') $ lFun lf, lSubFuns = start ++ end}
    where
      (start, subFun, end) = fromJust $ splitList i $ lSubFuns lf
      subFun' = setBit (j, v) subFun

splitList :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs = case end of
  []        -> Nothing
  (x: end') -> Just (start, x, end')
  where
    (start, end) = splitAt n xs

data LiftedSymmetric f g = LiftedSymmetric {
  lsFun     :: f,
  lsSubFuns :: MultiSet g
}

-- TODO-NEW handle constant g's
mkLiftedSymm :: f -> MultiSet g -> LiftedSymmetric f g
mkLiftedSymm = LiftedSymmetric

instance (BoFun f (), BoFun g j, Ord g) => BoFun (LiftedSymmetric f g) (Int, j) where
  isConst :: LiftedSymmetric f g -> Maybe Bool
  isConst = isConst . lsFun
  variables :: LiftedSymmetric f g -> [(Int, j)]
  variables lsf = do
    ((subFun, _), i) <- zip (MultiSet.toAscOccurList (lsSubFuns lsf)) naturals
    j <- variables subFun
    return (i, j)
  setBit :: ((Int, j), Bool) -> LiftedSymmetric f g -> LiftedSymmetric f g
  setBit ((i, j), v) lsf = case isConst subFun' of
    Nothing -> lsf{lsSubFuns = MultiSet.insert subFun' subFuns'}
    Just v' -> lsf{lsFun = setBit ((), v') $ lsFun lsf, lsSubFuns = subFuns'}
    where
      subFuns = lsSubFuns lsf
      (subFun, _) = MultiSet.toAscOccurList subFuns !! i
      subFuns' = MultiSet.delete subFun subFuns
      subFun' = setBit (j, v) subFun

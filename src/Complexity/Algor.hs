{-# LANGUAGE InstanceSigs #-}
module Complexity.Algor (
  Algor(..),
  DecTree(..)
) where

class Algor a  where
  res :: Bool -> a
  pic :: Int -> a -> a -> a

instance (Algor a, Algor b) => Algor (a, b) where
  res :: Bool -> (a, b)
  res = resBoth
  pic :: Int -> (a, b) -> (a, b) -> (a, b)
  pic = picBoth


resBoth :: (Algor a, Algor b) => Bool -> (a, b)
resBoth b = (res b, res b)
picBoth :: (Algor a, Algor b) => Int -> (a, b) -> (a, b) -> (a, b)
picBoth i (a0, b0) (a1, b1) = (pic i a0 a1, pic i b0 b1)

data DecTree = Res Bool | Pick Int DecTree DecTree
  deriving (Eq, Ord, Show)

instance Algor DecTree where
  res :: Bool -> DecTree
  res = Res
  pic :: Int -> DecTree -> DecTree -> DecTree
  pic = Pick


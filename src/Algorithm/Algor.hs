{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Algorithm.Algor (
  Algor(..),
  DecTree(..)
) where

class Algor a i | a -> i where
  res :: Bool -> a
  pic :: i -> a -> a -> a

instance (Algor a i, Algor b j) => Algor (a, b) (i, j) where
  res :: Bool -> (a, b)
  res = resBoth
  pic :: (i, j) -> (a, b) -> (a, b) -> (a, b)
  pic = picBoth


resBoth :: (Algor a i, Algor b j) => Bool -> (a, b)
resBoth b = (res b, res b)
picBoth :: (Algor a i, Algor b j) => (i, j) -> (a, b) -> (a, b) -> (a, b)
picBoth (i, j) (a0, b0) (a1, b1) = (pic i a0 a1, pic j b0 b1)

data DecTree = Res Bool | Pick Int DecTree DecTree
  deriving (Eq, Ord, Show)

instance Algor DecTree Int where
  res :: Bool -> DecTree
  res = Res
  pic :: Int -> DecTree -> DecTree -> DecTree
  pic = Pick

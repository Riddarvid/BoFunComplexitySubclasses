{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module Algorithm.Algor (
  Algor(..),
  DecTree(..)
) where

class Algor a where
  res :: Bool -> a
  pic :: i -> a -> a -> a

instance (Algor a, Algor b) => Algor (a, b) where
  res :: Bool -> (a, b)
  res = resBoth
  pic :: i -> (a, b) -> (a, b) -> (a, b)
  pic = picBoth


resBoth :: (Algor a, Algor b) => Bool -> (a, b)
resBoth b = (res b, res b)
picBoth :: (Algor a, Algor b) => i -> (a, b) -> (a, b) -> (a, b)
picBoth i (a0, b0) (a1, b1) = (pic i a0 a1, pic i b0 b1)

data DecTree = Res Bool | Pick Int DecTree DecTree
  deriving (Eq, Ord, Show)

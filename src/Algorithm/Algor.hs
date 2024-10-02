{-# LANGUAGE InstanceSigs #-}
module Algorithm.Algor (
  Algor(..),
  DecTree(..),
  allAlgors
) where
import           Data.Set (Set)
import qualified Data.Set as Set

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

-- Iterates pic until all the "variables" have been used.
allAlgors :: (Algor a, Ord a) => Int -> Set a
allAlgors n
  | n == 0 = Set.fromList [res False, res True]
  | otherwise = Set.fromList [pic n' a1 a2 | a1 <- subFuns, a2 <- subFuns]
  where
    n' = n - 1
    subFuns = Set.toList $ allAlgors n'

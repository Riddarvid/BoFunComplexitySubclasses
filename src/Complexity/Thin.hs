{-# LANGUAGE InstanceSigs #-}

-- Thinning, used by GenAlg
module Complexity.Thin (
  Thin(..)
) where

import qualified Data.Set as S
class Ord a => Thin a where
  cmp  :: a -> a -> Maybe Ordering

  covers    :: S.Set a -> a -> Bool
  thin      :: S.Set a -> S.Set a
  thinStep  :: S.Set a -> a -> S.Set a

  -- default definitions
  thin = S.foldl thinStep S.empty
  thinStep xs x = if covers xs x then xs else S.insert x xs -- TODO check if xs can be thinned
  covers ps q = S.member True (S.map (`check` q) ps)
    where check q' p = cmp q' p `elem` [Just LT, Just EQ]

instance (Thin a, Ord b) => Thin (a, b) where
  cmp :: (a, b) -> (a, b) -> Maybe Ordering
  cmp (a1, _) (a2, _) = cmp a1 a2

{-# LANGUAGE RankNTypes #-}
module Arity (
  ArbitraryArity(arbitraryArity),
  EnumerateArity(enumerateArity),
  AllArity(allArity)
) where
import           Control.Enumerable (Shareable, Sized, Typeable)
import           Data.Set           (Set)
import           Test.QuickCheck    (Gen)

class ArbitraryArity a where
  arbitraryArity :: Int -> Gen a

class EnumerateArity a where
  enumerateArity :: forall f. Int -> (Typeable f, Sized f) => Shareable f a

class AllArity a where
  allArity :: Int -> Set a

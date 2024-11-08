{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Arity (
  ArbitraryArity(arbitraryArity),
  EnumerateArity(enumerateArity),
  AllArity(allArity, allArityExplicit)
) where
import           Control.Enumerable (Shareable, Sized, Typeable)
import           Data.Set           (Set)
import           Test.QuickCheck    (Gen)

class ArbitraryArity a where
  arbitraryArity :: Int -> Gen a

class EnumerateArity a where
  enumerateArity :: forall f. Int -> (Typeable f, Sized f) => Shareable f a

-- Should generate all representations of arity n.
class AllArity a where
  allArity :: Int -> Set a
  allArityExplicit :: Int -> Set a
  allArityExplicit = allArity

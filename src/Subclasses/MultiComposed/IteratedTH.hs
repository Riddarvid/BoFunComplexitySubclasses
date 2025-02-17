{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Template Haskell for the Iterated' type
module Subclasses.MultiComposed.IteratedTH () where
import           Data.Function.Memoize        (Memoizable (memoize),
                                               deriveMemoize)
import           Subclasses.MultiComposed.Iterated (Iterated', SubFun)

instance (Memoizable (SubFun f)) => Memoizable (Iterated' f) where
  memoize :: (Iterated' f -> v) -> Iterated' f -> v
  memoize = $(deriveMemoize ''Iterated')

{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Subclasses.IteratedTH () where
import           Data.Function.Memoize (Memoizable (memoize), deriveMemoizable,
                                        deriveMemoize)
import           Subclasses.Iterated   (Iterated', SubFun)

instance (Memoizable (SubFun f)) => Memoizable (Iterated' f) where
  memoize :: (Iterated' f -> v) -> Iterated' f -> v
  memoize = $(deriveMemoize ''Iterated')

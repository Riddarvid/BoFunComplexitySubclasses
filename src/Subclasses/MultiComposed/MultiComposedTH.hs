{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Subclasses.MultiComposed.MultiComposedTH () where
import           Data.Function.Memoize                  (deriveMemoizable)
import           Subclasses.MultiComposed.MultiComposed (MultiComposed)

$(deriveMemoizable ''MultiComposed)

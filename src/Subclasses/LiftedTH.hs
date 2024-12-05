{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Subclasses.LiftedTH () where
import           Data.Function.Memoize (deriveMemoizable)
import           Subclasses.Lifted     (Lifted)

$(deriveMemoizable ''Lifted)

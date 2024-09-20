{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Subclasses.Id () where
import           BoFun (BoFun (..))

{-
* Nothing represents the identity function.
* Just val represents the constant function with value val.
-}
instance BoFun (Maybe Bool) () where
  isConst :: Maybe Bool -> Maybe Bool
  isConst = id
  variables :: Maybe Bool -> [()]
  variables = maybe [()] $ const []
  setBit :: ((), Bool) -> Maybe Bool -> Maybe Bool
  setBit ((), val) Nothing = Just val
  setBit _ _               = undefined

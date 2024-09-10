{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.Even () where
import           BoFun (BoFun (..))

data Even f = Even f Bool

instance BoFun f i => BoFun (Even f) i where
  isConst :: Even f -> Maybe Bool
  isConst (Even f inv) = case isConst f of
    Just res -> let res' = if inv then not res else res in Just res'
    Nothing  -> Nothing
  variables :: Even f -> [i]
  variables (Even f _) = variables f
  setBit :: (i, Bool) -> Even f -> Even f
  setBit = undefined

mkEven :: Bool -> f -> Even f
mkEven inv f = Even f inv

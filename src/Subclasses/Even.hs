{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Subclasses.Even () where
import           BoFun (BoFun (..))

data Even f = Even f Bool

instance BoFun f i => BoFun (Even f) i where
  isConst :: BoFun f i => Even f -> Maybe Bool
  isConst (Even f inv) = case isConst f of
    Just res -> let res' = if inv then not res else res in Just res'
    Nothing  -> Nothing
  variables :: BoFun f i => Even f -> [i]
  variables (Even f _) = variables f
  setBit :: BoFun f i => (i, Bool) -> Even f -> Even f
  setBit = undefined

-- Should be somewhere else in the code
evalBoFun :: BoFun f i => f -> [(i, Bool)] -> Maybe Bool
evalBoFun f vals = isConst f'
  where
    f' = foldr setBit f vals

mkEven :: BoFun f i => Bool -> f -> Even f
mkEven inv f = Even f inv

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.NormalizedCanonicalGenFun (
  NormalizedCanonicalGenFun,
  mkNCGF
) where
import           BoFun                       (BoFun (..))
import           Control.DeepSeq             (NFData)
import           Data.Function.Memoize       (deriveMemoizable)
import           Data.Hashable               (Hashable)
import           GHC.Generics                (Generic)
import           Subclasses.GenFun           (GenFun, toCanonicForm)
import           Subclasses.NormalizedGenFun (normalizeGenFun)

newtype NormalizedCanonicalGenFun = NCGF GenFun
  deriving (Generic, Eq, Show)

$(deriveMemoizable ''NormalizedCanonicalGenFun)

instance Hashable NormalizedCanonicalGenFun

instance NFData NormalizedCanonicalGenFun

mkNCGF :: GenFun -> NormalizedCanonicalGenFun
mkNCGF = NCGF . toCanonicForm . normalizeGenFun

unlift :: (GenFun -> a) -> NormalizedCanonicalGenFun -> a
unlift f (NCGF gf) = f gf

instance BoFun NormalizedCanonicalGenFun Int where
  isConst :: NormalizedCanonicalGenFun -> Maybe Bool
  isConst = unlift isConst
  variables :: NormalizedCanonicalGenFun -> [Int]
  variables = unlift variables
  setBit :: (Int, Bool) -> NormalizedCanonicalGenFun -> NormalizedCanonicalGenFun
  setBit v = NCGF . toCanonicForm . normalizeGenFun . unlift (setBit v)

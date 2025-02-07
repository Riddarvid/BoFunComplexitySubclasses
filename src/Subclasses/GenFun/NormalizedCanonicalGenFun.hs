{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.GenFun.NormalizedCanonicalGenFun (
  NormalizedCanonicalGenFun,
  mkNCGF
) where
import           Arity                              (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun                   (BoFun (..))
import           Control.DeepSeq                    (NFData)
import           Data.Function.Memoize              (deriveMemoizable)
import           Data.Hashable                      (Hashable)
import           GHC.Generics                       (Generic)
import           Subclasses.GenFun.CanonicalGenFun  (toCanonicForm)
import           Subclasses.GenFun.GenFun           (GenFun)
import           Subclasses.GenFun.NormalizedGenFun (normalizeGenFun)
import           Test.QuickCheck                    (Gen)

newtype NormalizedCanonicalGenFun = NCGF GenFun
  deriving (Generic, Eq, Show, Read)

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

instance ArbitraryArity NormalizedCanonicalGenFun where
  arbitraryArity :: Int -> Gen NormalizedCanonicalGenFun
  arbitraryArity arity = mkNCGF <$> arbitraryArity arity

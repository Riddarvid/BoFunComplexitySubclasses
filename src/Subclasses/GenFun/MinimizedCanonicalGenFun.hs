{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Subclasses.GenFun.MinimizedCanonicalGenFun (
  MinimizedCanonicalGenFun,
  mkNCGF
) where
import           Arity                             (ArbitraryArity (arbitraryArity))
import           Complexity.BoFun                  (BoFun (..))
import           Control.DeepSeq                   (NFData)
import           Data.Function.Memoize             (deriveMemoizable)
import           Data.Hashable                     (Hashable)
import           GHC.Generics                      (Generic)
import           Subclasses.GenFun.CanonicalGenFun (toCanonicForm)
import           Subclasses.GenFun.GenFun          (GenFun)
import           Subclasses.GenFun.MinimizedGenFun (normalizeGenFun)
import           Test.QuickCheck                   (Gen)

newtype MinimizedCanonicalGenFun = NCGF GenFun
  deriving (Generic, Eq, Show, Read)

$(deriveMemoizable ''MinimizedCanonicalGenFun)

instance Hashable MinimizedCanonicalGenFun

instance NFData MinimizedCanonicalGenFun

mkNCGF :: GenFun -> MinimizedCanonicalGenFun
mkNCGF = NCGF . toCanonicForm . normalizeGenFun

unlift :: (GenFun -> a) -> MinimizedCanonicalGenFun -> a
unlift f (NCGF gf) = f gf

instance BoFun MinimizedCanonicalGenFun Int where
  isConst :: MinimizedCanonicalGenFun -> Maybe Bool
  isConst = unlift isConst
  variables :: MinimizedCanonicalGenFun -> [Int]
  variables = unlift variables
  setBit :: (Int, Bool) -> MinimizedCanonicalGenFun -> MinimizedCanonicalGenFun
  setBit v = NCGF . toCanonicForm . normalizeGenFun . unlift (setBit v)

instance ArbitraryArity MinimizedCanonicalGenFun where
  arbitraryArity :: Int -> Gen MinimizedCanonicalGenFun
  arbitraryArity arity = mkNCGF <$> arbitraryArity arity

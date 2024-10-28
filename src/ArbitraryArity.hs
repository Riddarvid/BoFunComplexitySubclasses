module ArbitraryArity (ArbitraryArity(arbitraryArity)) where
import           Test.QuickCheck (Gen)

class ArbitraryArity a where
  arbitraryArity :: Int -> Gen a

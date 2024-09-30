module MemoizeTests (
  myDeriveMemoize
) where
import           Data.Function.Memoize (Memoizable (memoize))

myDeriveMemoize :: Memoizable b => (a -> b) -> (b -> a) -> (a -> v) -> a -> v
myDeriveMemoize deconstruct construct f = memoize (f . construct) . deconstruct

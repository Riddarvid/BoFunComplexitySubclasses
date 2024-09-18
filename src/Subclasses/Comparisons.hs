module Subclasses.Comparisons (benchBoFun, complexityBench) where
import           Algorithm.GenAlgPW    (computeMin)
import           BoFun                 (BoFun)
import           Criterion             (Benchmark, bench, bgroup, nf)
import           Criterion.Main        (defaultMain)
import           Data.Function.Memoize (Memoizable)

benchBoFun :: String -> [Benchmark] -> IO ()
benchBoFun name benchmarks = defaultMain [bgroup name benchmarks]

complexityBench :: (BoFun f i, Memoizable f) => String -> f -> Benchmark
complexityBench name f = bench name $ nf computeMin f

module Main (main) where
import           ComputeAll (genAllBoths)

main :: IO ()
main = mapM_ print $ genAllBoths 3

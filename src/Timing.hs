{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Timing (measureMajs, measureFuns) where
import           Data.List   (sort)
import           Data.Time   (NominalDiffTime)
import           Debug.Trace (traceShow, traceShowId)

measureMajs :: (f -> IO NominalDiffTime) -> (Int -> f) -> Int -> Int -> IO [(Int, NominalDiffTime)]
measureMajs timingFun majFun samples n = zip bits <$> measureFuns timingFun funs samples
  where
    bits = [1, 3 .. n]
    funs = map majFun bits

measureFuns :: (f -> IO NominalDiffTime) -> [f] -> Int -> IO [NominalDiffTime]
measureFuns timingFun funs samples = sequenceA measurements
  where
    measurements = map (medianMeasure timingFun samples) funs

medianMeasure :: (f -> IO NominalDiffTime) -> Int -> f -> IO NominalDiffTime
medianMeasure timingFun n f = do
  times <- sequenceA timingActions
  return $ median times
  where
    timingActions = replicate n (timingFun f)

median :: [NominalDiffTime] -> NominalDiffTime
median times
  | odd lt = times' !! mid
  | otherwise = (times' !! mid' + times' !! mid) / 2
  where
    times' = sort times
    lt = length times'
    mid = lt `div` 2
    mid' = mid - 1

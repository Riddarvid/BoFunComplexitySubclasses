
An iterated threshold fun is defined as:

IteratedThresholdFun f = Free ThresholdFun f

Which in turn means

Pure f | Free (ThresholdFun IteratedThresholdFun)

SO either we simply have a single function f, or we have a threshold function whose sub-functions are threshold functions, thus creating the iterative property.

Further, we have 

type IteratedThresholdFun' = IteratedThresholdFun ()

Giving us 

Pure () | Free (ThresholdFun IteratedThresholdFun')

which is isomorphic with

Maybe (ThresholdFun IteratedThresholdFun')
Here we see that about 3% of the time was spent on isConst and 0.4% on setBit.

With our new Seq+range based approach, isConst is so small that it is negligible, while setBit takes 1% of the time. 

Looking at the flamegraph it is now obvious that we could not have achieved more than a 4% speedup, so this was perhaps an unnecessary optimization.
If we look at computeMin, we see that more than 50% of the time is spent on functions relating to memoization. We see that it is not the inSig/outSig function calls that take time, it is the lookups used in the memoization.

Polynomial/piecewise polynomial operations take up about 15% of the time. The total number of polynomial operations probably doesn't change between implementations, as they are only dependent on the actual generated decision trees.

The final chunk of time is used by setBit, which in turn calls restrict on a BDD. This takes up about 20% of the time.


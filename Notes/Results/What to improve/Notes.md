We start by noting that the things we can affect are the memoization function and the BoFun functions.

In general, we see that in the general BDD implementation, around 70% of the time is spent on memoization and the BoFun-operations.

If we instead look at threshold functions, we see that this part has shrunk to around 10% of the time (should be a x2.5 speedup but need to double check this). A large part of this is spent on memoization.

For symmetric functions we see that the same segment takes up around 15% of the time, with the vast majority of that time being spent on memoization.

## Further improvements

Looking at threshold functions, we see that around 90% of the time is spent on polynomial operations. This is time we cannot change, at least with our current approach of writing new types that are instances of BoFun and Memoizable. This means that no matter how much we improve the representation of threshold functions, we can only achieve a x1.1 speedup at most. This is probably not worth it.

Otherwise, writing better representations for other classes of functions can of course yield a speedup similar to the one between threshold and BDD. 

Another approach is to make a faster implementation of general functions. Right now we use BDDs, but we are looking into BDD forests as a potential alternative. We are also examining mirrorings, using the fact that D(f) = D(!f). It could also be that there exists some even more efficient implementation of general boolean functions than BDDs. Improving this would result in a speedup for all functions, not just for a certain class of functions.
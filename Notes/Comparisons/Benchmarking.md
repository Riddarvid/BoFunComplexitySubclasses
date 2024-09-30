Note: The first results using criterion seem unreliable.

## Using non-normalized BDDs:

benchmarking maj11/symmetric basicmaj11
time                 176.2 ns   (175.7 ns .. 176.7 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 180.4 ns   (178.6 ns .. 182.8 ns)
std dev              7.161 ns   (4.876 ns .. 10.00 ns)
variance introduced by outliers: 59% (severely inflated)

benchmarking maj11/symmetric maj11
time                 1.322 μs   (1.314 μs .. 1.334 μs)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 1.333 μs   (1.319 μs .. 1.396 μs)
std dev              76.49 ns   (23.75 ns .. 164.3 ns)
variance introduced by outliers: 71% (severely inflated)

benchmarking maj11/threshold maj11
time                 812.6 ns   (810.6 ns .. 815.7 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 821.2 ns   (815.3 ns .. 843.3 ns)
std dev              33.97 ns   (8.611 ns .. 69.97 ns)
variance introduced by outliers: 58% (severely inflated)

benchmarking maj11/generic maj11
time                 687.7 μs   (650.7 μs .. 764.6 μs)
                     0.821 R²   (0.720 R² .. 0.925 R²)
mean                 937.1 μs   (854.9 μs .. 1.062 ms)
std dev              328.9 μs   (275.7 μs .. 405.5 μs)
variance introduced by outliers: 99% (severely inflated)

## Using normalized BDDs

benchmarking maj11/symmetric basicmaj11
time                 177.7 ns   (175.0 ns .. 181.0 ns)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 178.2 ns   (176.8 ns .. 180.3 ns)
std dev              5.963 ns   (4.093 ns .. 8.853 ns)
variance introduced by outliers: 50% (severely inflated)

benchmarking maj11/symmetric maj11
time                 1.341 μs   (1.337 μs .. 1.345 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.357 μs   (1.346 μs .. 1.381 μs)
std dev              58.68 ns   (24.11 ns .. 105.2 ns)
variance introduced by outliers: 59% (severely inflated)

benchmarking maj11/threshold maj11
time                 813.9 ns   (809.9 ns .. 818.5 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 816.0 ns   (809.9 ns .. 825.5 ns)
std dev              23.87 ns   (14.81 ns .. 39.51 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking maj11/generic maj11
time                 654.6 μs   (643.5 μs .. 664.5 μs)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 690.3 μs   (672.3 μs .. 733.0 μs)
std dev              95.44 μs   (52.81 μs .. 152.8 μs)
variance introduced by outliers: 85% (severely inflated)

## Single measurements without normalization:

0.000471016s
0.001050164s
0.000720129s
9.171595347s

## Single measurements with normalization:

0.000388456s
0.000924568s
0.000659869s
0.094150311s
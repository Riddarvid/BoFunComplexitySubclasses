
## maj9

2024-09-18

benchmarking maj9/symmetric maj9
time                 174.9 ns   (167.8 ns .. 181.2 ns)
                     0.988 R²   (0.976 R² .. 0.997 R²)
mean                 170.7 ns   (165.8 ns .. 179.1 ns)
std dev              19.48 ns   (10.29 ns .. 29.19 ns)
variance introduced by outliers: 93% (severely inflated)

benchmarking maj9/threshold maj9
time                 794.8 ns   (781.0 ns .. 817.7 ns)
                     0.968 R²   (0.937 R² .. 0.986 R²)
mean                 1.080 μs   (946.8 ns .. 1.266 μs)
std dev              538.8 ns   (327.4 ns .. 756.7 ns)
variance introduced by outliers: 100% (severely inflated)

benchmarking maj9/generic maj9
time                 131.8 μs   (129.9 μs .. 134.8 μs)
                     0.997 R²   (0.993 R² .. 1.000 R²)
mean                 131.8 μs   (130.7 μs .. 133.9 μs)
std dev              5.085 μs   (2.946 μs .. 9.137 μs)
variance introduced by outliers: 38% (moderately inflated)
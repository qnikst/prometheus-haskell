# Benchmark.

We do not use criterion or similar tools here because the code
will be affected by the GHC metrics.

You can run benchmarks by running first building `Main.hs`, I do that
by running:

```
cabal v2-exec ghc -- Main.hs -package prometheus-metrics-ghc
```

Then run script:

```
cabal v2-run criterion.hs --
   -o report.html \
  --csv report.csv \
  --json report.json
```

The expectation is that run time of the program is not affected by the metrics.
Current metrics check only time spent on the metrics maintenace, and doesn't
check how metrics output affect the running time.

The fib function is chosen because it heavily affects GC, so if gc hook adds
measurable overhead then it will be seen in the benchmark.



The results that I had: 


benchmarking 10/no-metrics
ggtime                 12.19 s    (11.39 s .. 12.89 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 12.41 s    (12.24 s .. 12.55 s)
std dev              173.6 ms   (63.70 ms .. 222.6 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 10/metrics
time                 12.21 s    (12.05 s .. 12.34 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.19 s    (12.14 s .. 12.21 s)
std dev              45.96 ms   (16.45 ms .. 62.65 ms)
variance introduced by outliers: 19% (moderately inflated)

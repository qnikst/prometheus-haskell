# Prometheus extra

This package provides additional functionality that is missing from the
[prometheus-client](https://github.com/qnikst/prometheus-client) package
and is not advertised in the official clients but still may benefit your
application.

## Contents

1. <a href="#integer-counter">Integer counters</a>
2. <a href="#integer-gauge">Integer gauge</a>

## <a id="integer-counter"> Integer counter</a>

Integer counter is a special counter that have exactly the same API as an
ordinary counter, but it works with integral values. It allows much more
efficient implementation. If you are using integral values then `IntCounter`
may be a drop-in replacement:

```haskell
import Prometheus.Metric.IntCounter -- was import Prometheus.Metric.Counter

function = do
  c <- register $ counter $ Info "name" "help string"
  incrCounter c
```

Counter is implemented based on [atomic-primops](https://hackage.haskell.org/package/atomic-primops)
and allow non-blocking inplace updates, so counter works in the constant
memory, on the contrary to an ordinary 'Counter' in the prometheus-client.


Current benchmarks are not very representative as evaluation is quite fast
and counters is accessed by single thread only. But the results are that
IntCounter is about 10% faster. But we demand better benchmarks here.

```
| IntCounter                                              | Counter
| benchmarking IntCounter/incCounter                      | benchmarking Counter/incCounter                          |
|time                 17.00 ns   (16.78 ns .. 17.35 ns)   | time                 18.82 ns   (18.69 ns .. 19.04 ns)   |
|                     0.998 R²   (0.996 R² .. 1.000 R²)   |                      0.999 R²   (0.997 R² .. 1.000 R²)   |
|mean                 16.96 ns   (16.78 ns .. 17.25 ns)   | mean                 19.17 ns   (18.95 ns .. 19.61 ns)   |
|std dev              738.2 ps   (456.3 ps .. 1.006 ns)   | std dev              969.0 ps   (615.2 ps .. 1.415 ns)   |
|variance introduced by outliers: 67% (severely inflated) | variance introduced by outliers: 73% (severely inflated) |
```

```
| benchmarking IntCounter/addCounter                         | benchmarking Counter/addCounter
|time                 17.95 ns   (17.87 ns .. 18.05 ns)      | time                 19.33 ns   (19.25 ns .. 19.41 ns)
|                     1.000 R²   (1.000 R² .. 1.000 R²)      |                      1.000 R²   (0.999 R² .. 1.000 R²)
|mean                 18.01 ns   (17.93 ns .. 18.11 ns)      | mean                 19.41 ns   (19.29 ns .. 19.66 ns)
|std dev              299.2 ps   (247.3 ps .. 443.1 ps)      | std dev              579.5 ps   (326.9 ps .. 996.0 ps)
|variance introduced by outliers: 23% (moderately inflated)  | variance introduced by outliers: 49% (moderately inflated)
```

## <a id="integer-gauge"> Integer gauge</a>

Special gauge that shares API with it's ordinary counterpart, but it works
with `Int` values instead. It allows to have more precise intention about the
values and use efficient CPU instructions and 'inplace' implementation.

Performace is basically 10% faster comparing to the ordinary Gauge, but it
doesn't allocate memory when storing the values.



# GHC Prometheurs metrics

This package provides simple way to get application metrics using prometheus.


In order to use this library you need to register hook in the main file.

```haskell
import Prometheus
import Prometheus.Metric.GHC


main = do
  _ <- register ghcMetrics
  ...
```

After setting this you'll get an access to the methics provided by the Haskell
Runtime system.

You may find list of all metrics in the haddock. (TODO: link)


In addition to the raw values you may want to add additional composite metrics:

1. Time spent in GC:

```
rate(ghc_gc_elapsed_seconds_total[2m]) / (rate(ghc_gc_elapsed_seconds_total[2m])+rate(ghc_mutator_elapsed_seconds_total[2m]))
```
For ordinary web service is likely is lower than `0.1%`. (TODO add picture)

2. Average GC time (TODO: update with new stats):

```
increase(ghc_gc_cpu_seconds_total[5m])/increase(ghc_gcs_total[5m])
```

(TODO add picture)


Opinionated choices.

This package differ from vanilla as it introduces additional logic for getting
all intermediate GC information. Instead of momentary values from gcdetails it
stores maximum values for each generation and clears values each time metrics
is gathered so you may get some insight on what is happening between the metrics.
Currently we just use maximum values but in the future it's possible to use
Statistics or Histogram values.


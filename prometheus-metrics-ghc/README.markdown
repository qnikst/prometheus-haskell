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


Useful metrics to output:

TODO: make nice pictures.



Internals.

This package differ from vanilla as it introduces additional logic for getting
all intermediate GC information. Instead of momentary values from gcdetails it
stores maximum values for each generation and clears values each time metrics
is gathered so you may get some insight on what is happening between the metrics.
Currently we just use maximum values but in the future it's possible to use
Statistics or Histogram values.



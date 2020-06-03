# GHC Prometheurs metrics

The package provides simple way to export application Garbage Collection metrics to [Prometheus](https://prometheus.io/).

In order to use this library you need to register hook in the main file:

```haskell
import Prometheus
import Prometheus.Metric.GHC

main = do
  _ <- register ghcMetrics
  ... application logic
```

To make metrics available you need to add add `+RTS -T` to the program command line:
```
./my-program +RTS -T -RTS
```
Or use system environment variable `GHCRTS`:
```
GHCRTS=-T ./my-program
```

Then you can output metrics by any available method (check [prometheus-client documentation](https://hackage.haskell.org/package/prometheus-client)
for the full list).

## List of supported metrics.

**Totals**: 

 Option | Description |
  ---   | :--- |
`ghc_gcs_total` | Total number of GCs
`ghc_major_gcs_total` | Total number of major (oldest generation) GCs 
`ghc_allocated_bytes_total` | Total bytes allocated
`ghc_cumulative_live_bytes_total` | Sum of live bytes across all major GCs. Divided by `ghc_major_gcs` gives the average live data over the lifetime of the program.
`ghc_copied_bytes_total` | Sum of copied bytes across all GCs
`ghc_par_copied_bytes_total` | Sum of copied bytes across all parallel GCs
`ghc_cumulative_par_max_copied_bytes_total` | Sum of `par_max_copied_bytes` across all parallel GCs
`ghc_mutator_cpu_seconds_total` | Total CPU time used by the program outsize GC (mutator)
`ghc_mutator_elapsed_seconds_total` | Total elapsed (wall clock) time that the program was working outside of GC (mutator)
`ghc_gc_cpu_seconds_total` | Total CPU time used by the GC
`ghc_gc_elapsed_seconds_total` | Total elapsed (wall clock) time program spent in GC
`ghc_cpu_seconds_total` | Total CPU time (at the previous GC)
`ghc_elapsed_seconds_total` | Total elapsed time (at the previous GC)

**Maximum values**:

 Option | Description |
  ---   | :--- |
`ghc_max_live_bytes` | Maximum live data (including large objects + compact regions)
`ghc_max_large_objects_bytes` | Maximum live data in large objects
`ghc_max_compact_bytes` | Maximum live data in [compact regions](https://hackage.haskell.org/package/compact)
`ghc_max_slop_bytes` | Maximum slop unused memory allocated due to technical purposes: block sizes, alignments.
`ghc_max_mem_in_use_bytes` | Maximum memory in use by the RTS

**Per generation values**:
   All values below have `gen` label telling what generation the value belongs to. So you can select or
   reduce values by their generation. Provided statistics is a maximum value that was seen between
   prometheus scrapes (see discussion in Optinionated Choices section).

  Option | Description |
   ---   | :--- 
 `ghc_gcdetails_allocated_bytes` | Number of bytes allocated since the previous GC
 `ghc_gcdetails_live_bytes` | Total amount of live data in the heap (including large + compact data)
 `ghc_gcdetails_large_objects_bytes` | Total amount of live data in large objects
 `ghc_gcdetails_compact_bytes` | Total amount of live data in compact regions
 `ghc_gcdetails_slop_bytes` | Total amount of slop (wasted memory)
 `ghc_gcdetails_mem_in_use_bytes` | Total amount of memory in use by the RTS
 `ghc_gcdetails_copied_bytes` | Total amount of data copied during this GC
 `ghc_gcdetails_par_max_copied_bytes` | In parallel GC, the max amount of data copied by any one thread
 `ghc_gcdetails_sync_elapsed_seconds` | The time elapsed during synchronisation before GC
 `ghc_gcdetails_cpu_seconds` | The CPU time used during GC itself
 `ghc_gcdetails_elapsed_seconds` | The time elapsed during GC itself
 `ghc_gcdetails_elapsed_seconds` | The time elapsed during GC itself

If for any reason there is metric that 
is package does not provide but it exists, feel free to open a [bug report](http://github.com/qnikst/prometheus-client/issues).
Full list of possible metrics is available in base [haddock](https://hackage.haskell.org/package/base/docs/GHC-Stats.html). 


# Tips and tricks.

In addition to the raw values you may want to add additional composite metrics:

1. Time spent in GC:

```
rate(ghc_gc_elapsed_seconds_total[2m]) / (rate(ghc_gc_elapsed_seconds_total[2m])+rate(ghc_mutator_elapsed_seconds_total[2m]))
```
For ordinary web service is likely is lower than `0.1%`.
![](img/1.png)

2. Average GC time (TODO: update with new stats):

```
increase(ghc_gc_cpu_seconds_total[5m])/increase(ghc_gcs_total[5m])
```

![](img/2.png)

3. GC parallelism.
```
rate(ghc_gc_cpu_seconds_total[2m])/rate(ghc_gc_elapsed_seconds_total[2m])
```
Tells if there was a benefit from the parallel GC.

## Performance considerations.

Current package does not affect performance of the program. Metrics are gathered in
a constant memory with a small abount of cpu work that is much smaller than cpu work
during GC. If you are interseted in more details you may check [benchmark section](https://github.com/qnikst/prometheus-client/prometheus-metrics-ghc/benchmarks/Readme.markdown).

## Contributing.

All contributions and PR are weclome, improvements in documentation, usecases
and grafana and other visualization tools are welcome.

## Resources

If you are interested in how Garbage Collector and Runtime works in Haskell you 
may want to read.
1. [Runtime description on GHC site](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts)
2. [GHC Runtime in pictures](http://www.scs.stanford.edu/14sp-cs240h/slides/ghc-rts.pdf)
3. [Parallel Generational-Copying Garbage Collection with a Block-Structured Heap](http://simonmar.github.io/bib/papers/parallel-gc.pdf)


## Opinionated choices.

This package differ from vanilla as it introduces additional logic for getting
all intermediate GC information. Instead of momentary values from gcdetails it
stores maximum values for each generation and clears values each time metrics
is gathered so you may get some insight on what is happening between the metrics.
Currently we just use maximum values but in the future it's possible to use
Statistics or Histogram values.


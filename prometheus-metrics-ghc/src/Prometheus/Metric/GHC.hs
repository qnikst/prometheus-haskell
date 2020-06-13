{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language NumDecimals #-}
-- | This module defines a metrics that exposes statistics from the GHC runtime
-- system ("GHC.Conc", "GHC.Stats").
--
-- To use these metrics, the monitored executable should run with the `+RTS -T`
-- command line flags and the following must be added somewhere near the
-- beginning of the main method:
--
module Prometheus.Metric.GHC (
    GHCMetrics
,   ghcMetrics
,   ghcMetricsWithLabels
,   setGCHook
) where

import qualified Data.ByteString.UTF8 as BS
import Data.Text (Text, pack)
import Data.Fixed (Fixed, E9)
import GHC.Stats (RTSStats(..), getRTSStatsEnabled, getRTSStats)
import qualified GHC.Stats as Stats
import Prometheus
import Prometheus.Metric.GHC.Internal

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prometheus

-- | Garbage collection metrics handle.
data GHCMetrics = GHCMetrics

-- | Basic ghc metrics, without extra labels.
--
-- >>> register ghcMetrics
ghcMetrics :: Metric GHCMetrics
ghcMetrics = ghcMetricsWithLabels []

-- | GHC metrics accompanied with provided metrics. Such
-- metrics can be used for later analysis.
--
-- >>> register $ ghcMetricsWithLabels [("environment", "production")]
--
ghcMetricsWithLabels :: LabelPairs -> Metric GHCMetrics
ghcMetricsWithLabels labels = Metric $ do
  statsEnabled <-
    getRTSStatsEnabled
  if statsEnabled
  then do
    setGCHook
    return (GHCMetrics, do
        stats <- getRTSStats
        (extra, gens) <- getExtraStats
        clearExtraData 
        pure $ ghcCollectors labels stats
          <> gcExtra labels extra
          <> concat (zipWith (gcDetails labels) [0..] gens))
  else return (GHCMetrics, return [])

gcExtra
  :: LabelPairs
  -> GCExtra
  -> [SampleGroup]
gcExtra labels GCExtra{..} = 
   [ statsCollector
      "ghc_gcdetails_live_bytes"
      "Total amount of live data in the heap (including large + compact data)"
      GaugeType
      gcdetails_live_bytes
   , statsCollector
      "ghc_gcdetails_large_objects_bytes"
      "Total amount of live data in large objects"
      GaugeType
      gcdetails_large_objects_bytes
   , statsCollector
      "ghc_gcdetails_compact_bytes"
      "Total amount of live data in compact regions"
      GaugeType
      gcdetails_compact_bytes
   , statsCollector
      "ghc_gcdetails_slop_bytes"
      "Total amount of slop (wasted memory)"
      GaugeType
      gcdetails_slop_bytes
   , statsCollector
      "ghc_gcdetails_mem_in_use_bytes"
      "Total amount of memory in use by the RTS"
      CounterType
      gcdetails_mem_in_use_bytes
   ]
  where
    statsCollector :: Show a
               => Text -> Text -> SampleType -> a -> SampleGroup
    statsCollector = showCollector labels

-- | Details about each GC.
gcDetails
  :: LabelPairs -- ^ Default labels
  -> Int -- ^ Generation number
  -> GCExtraGen -- ^ Extra GC info structure
  -> [SampleGroup]
gcDetails labels gen GCExtraGen{..} = 
    [ statsCollector
        "ghc_gcdetails_allocated_max_bytes"
        "Number of bytes allocated since the previous GC"
        GaugeType
        gcdetails_allocated_max_bytes
    , statsCollector
        "ghc_gcdetails_allocated_total_bytes"
        "Number of bytes allocated since the previous GC"
        CounterType
        gcdetails_allocated_total_bytes
    , statsCollector
        "ghc_gcdetails_copied_max_bytes"
        "Total amount of data copied during this GC"
        GaugeType
        gcdetails_copied_max_bytes
    , statsCollector
        "ghc_gcdetails_copied_total_bytes"
        "Total amount of data copied during this GC"
        CounterType
        gcdetails_copied_total_bytes
    , statsCollector
        "ghc_gcdetails_par_max_copied_bytes"
        "In parallel GC, the max amount of data copied by any one thread"
        GaugeType
        gcdetails_par_max_copied_bytes
    , statsCollector
        "ghc_gcdetails_cpu_max_seconds"
        "The CPU time used during GC itself"
        GaugeType
        (rtsTimeToSeconds gcdetails_cpu_max_ns)
    , statsCollector
        "ghc_gcdetails_cpu_total_seconds"
        "The CPU time used during GC itself"
        CounterType
        (rtsTimeToSeconds gcdetails_cpu_total_ns)
    , statsCollector
        "ghc_gcdetails_elapsed_max_seconds"
        "The time elapsed during GC itself"
        GaugeType
        (rtsTimeToSeconds gcdetails_elapsed_max_ns)
    , statsCollector
        "ghc_gcdetails_elapsed_total_seconds"
        "The time elapsed during GC itself"
        CounterType
        (rtsTimeToSeconds gcdetails_elapsed_total_ns)
    , statsCollector
        "ghc_gcdetails_sync_elapsed_max_seconds"
        "The time elapsed during GC itself"
        GaugeType
        (rtsTimeToSeconds gcdetails_sync_elapsed_max_ns)
    , statsCollector
        "ghc_gcdetails_sync_elapsed_total_seconds"
        "The time elapsed during GC itself"
        CounterType
        (rtsTimeToSeconds gcdetails_sync_elapsed_total_ns)
    ]
  where
    statsCollector :: Show a
               => Text -> Text -> SampleType -> a -> SampleGroup
    statsCollector = showCollector (("gen", pack (show gen)):labels)

-- | Common GC values.
ghcCollectors :: LabelPairs -> RTSStats -> [SampleGroup]
ghcCollectors labels RTSStats{..} = [
      statsCollector
            "ghc_gcs_total"
            "Total number of GCs"
            CounterType
            gcs
    , statsCollector
            "ghc_major_gcs_total"
            "Total number of major (oldest generation) GCs"
            CounterType
            major_gcs
    , statsCollector
            "ghc_allocated_bytes_total"
            "Total bytes allocated"
            CounterType
            allocated_bytes
    , statsCollector
            "ghc_max_live_bytes"
            "Maximum live data (including large objects + compact regions)"
            GaugeType
            max_live_bytes
    , statsCollector
            "ghc_max_large_objects_bytes"
            "Maximum live data in large objects"
            GaugeType
            max_large_objects_bytes
    , statsCollector
            "ghc_max_compact_bytes"
            "Maximum live data in compact regions"
            GaugeType
            max_compact_bytes
    , statsCollector
            "ghc_max_slop_bytes"
            "Maximum slop"
            GaugeType
            max_slop_bytes
    , statsCollector
            "ghc_max_mem_in_use_bytes"
            "Maximum memory in use by the RTS"
            GaugeType
            max_mem_in_use_bytes
    , statsCollector
            "ghc_cumulative_live_bytes_total"
            "Sum of live bytes across all major GCs. Divided by ghc_gc_major_gcs_total gives the average live data over the lifetime of the program."
            CounterType
            cumulative_live_bytes
    , statsCollector
            "ghc_copied_bytes_total"
            "Sum of copied_bytes across all GCs"
            CounterType
            copied_bytes
    , statsCollector
            "ghc_par_copied_bytes_total"
            "Sum of copied_bytes across all parallel GCs"
            CounterType
            par_copied_bytes
    , statsCollector
            "ghc_cumulative_par_max_copied_bytes_total"
            "Sum of par_max_copied_bytes across all parallel GCs"
            CounterType
            cumulative_par_max_copied_bytes
    , statsCollector
            "ghc_mutator_cpu_seconds_total"
            "Total CPU time used by the mutator"
            CounterType
            (rtsTimeToSeconds mutator_cpu_ns)
    , statsCollector
            "ghc_mutator_elapsed_seconds_total"
            "Total elapsed time used by the mutator"
            CounterType
            (rtsTimeToSeconds mutator_elapsed_ns)
    , statsCollector
            "ghc_gc_cpu_seconds_total"
            "Total CPU time used by the GC"
            CounterType
            (rtsTimeToSeconds gc_cpu_ns)
    , statsCollector
            "ghc_gc_elapsed_seconds_total"
            "Total elapsed time used by the GC"
            CounterType
            (rtsTimeToSeconds gc_elapsed_ns)
    , statsCollector
            "ghc_cpu_seconds_total"
            "Total CPU time (at the previous GC)"
            CounterType
            (rtsTimeToSeconds cpu_ns)
    , statsCollector
            "ghc_elapsed_seconds_total"
            "Total elapsed time (at the previous GC)"
            CounterType
            (rtsTimeToSeconds elapsed_ns)
  ]
  where
    statsCollector :: Show a => Text -> Text -> SampleType -> a -> SampleGroup
    statsCollector = showCollector labels 

-- | Convert from 'RtsTime' (nanoseconds) to seconds with nanosecond precision.
rtsTimeToSeconds :: Stats.RtsTime -> Fixed E9
rtsTimeToSeconds = (/ 1e9) . fromIntegral

showCollector :: Show a => LabelPairs -> Text -> Text -> SampleType -> a -> SampleGroup
showCollector labels name help sampleType value =
    let info = Info name help
        valueBS = BS.fromString $ show value
    in SampleGroup info sampleType [Sample name labels valueBS]

-- | Setup additional hooks.
foreign import ccall "set_extra_gc_hook" setGCHook :: IO ()

-- | Clear statistics.
foreign import ccall unsafe "extra_gc_stats_clear" clearExtraData :: IO ()

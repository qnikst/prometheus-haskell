-- | Internal datastructure for additional data.
-- This structure mostly copy 'RTStats' structure from base.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Prometheus.Metric.GHC.Internal
  ( getExtraStats
  , GCExtra(..)
  , GCExtraGen(..)
  ) where

import Data.Int
import Data.Word
import GHC.Generics (Generic)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

#include "gc_extra.h"

-- | Populate structure.
foreign import ccall "populate_gc_extra" populate_gc_extra :: Ptr () -> Ptr () -> IO ()

-- | Extra data about memory
data GCExtra = GCExtra
  { gcdetails_live_bytes :: Word64
    -- | Total amount of live data in large objects
  , gcdetails_large_objects_bytes :: Word64
    -- | Total amount of live data in compact regions
  , gcdetails_compact_bytes :: Word64
    -- | Total amount of slop (wasted memory)
  , gcdetails_slop_bytes :: Word64
    -- | Total amount of memory in use by the RTS
  , gcdetails_mem_in_use_bytes :: Word64
  }

-- | Extra data about each GC generation.
data GCExtraGen = GCExtraGen {
    -- | The generation number of this GC
    gcdetails_gen :: Word32
  , gcdetails_count :: Word64
    -- | Total amount of live data in the heap (includes large + compact data).
    -- Updated after every GC. Data in uncollected generations (in minor GCs)
    -- are considered live.
  , gcdetails_allocated_max_bytes :: Word64
  , gcdetails_allocated_total_bytes :: Word64
    -- | Total amount of data copied during this GC
  , gcdetails_copied_max_bytes :: Word64
  , gcdetails_copied_total_bytes :: Word64
    -- | In parallel GC, the max amount of data copied by any one thread.
    -- Deprecated.
  , gcdetails_par_max_copied_bytes :: Word64
    -- | In parallel GC, the amount of balanced data copied by all threads
  , gcdetails_par_balanced_copied_bytes :: Word64
    -- | The time elapsed during synchronisation before GC
  , gcdetails_sync_elapsed_total_ns :: RtsTime
  , gcdetails_sync_elapsed_max_ns :: RtsTime
    -- | The CPU time used during GC itself
  , gcdetails_cpu_total_ns :: RtsTime
  , gcdetails_cpu_max_ns :: RtsTime
    -- | The time elapsed during GC itself
  , gcdetails_elapsed_total_ns :: RtsTime
  , gcdetails_elapsed_max_ns :: RtsTime
  } deriving ( Read
             , Show
             , Generic
             )

getExtraStats :: IO (GCExtra, [GCExtraGen])
getExtraStats = do
  allocaBytes (#size GCExtra) $ \p0 -> do
    allocaBytes (2 * #size GCExtraGen) $ \p1 -> do
      populate_gc_extra p1 p0
      extra <- fetchExtra p0
      gc0 <- fetch 0 p1
      gc1 <- fetch 1 (p1 `plusPtr` #size GCExtraGen)
      return (extra, [gc0, gc1])
    where
      fetchExtra p = do
        gcdetails_live_bytes <- (# peek GCExtra, live_bytes_max) p
        gcdetails_large_objects_bytes <-
          (# peek GCExtra, large_objects_bytes_max) p
        gcdetails_compact_bytes <- (# peek GCExtra, compact_bytes_max) p
        gcdetails_slop_bytes <- (# peek GCExtra, slop_bytes_max) p
        gcdetails_mem_in_use_bytes <- (# peek GCExtra, mem_in_use_bytes_max) p
        pure GCExtra{..}
      fetch i p = do
        let gcdetails_gen = i
        gcdetails_count <- (# peek GCExtraGen, count) p
        gcdetails_allocated_max_bytes <- (# peek GCExtraGen, allocated_bytes_max) p
        gcdetails_allocated_total_bytes <- (# peek GCExtraGen, allocated_bytes_total) p
        gcdetails_copied_max_bytes <- (# peek GCExtraGen, copied_bytes_max) p
        gcdetails_copied_total_bytes <- (# peek GCExtraGen, copied_bytes_total) p
        gcdetails_par_max_copied_bytes <-
          (# peek GCExtraGen, par_max_copied_bytes) p
        gcdetails_par_balanced_copied_bytes <-
          (# peek GCExtraGen, par_balanced_copied_bytes) p
        gcdetails_cpu_max_ns <- (# peek GCExtraGen, cpu_ns_max) p
        gcdetails_cpu_total_ns <- (# peek GCExtraGen, cpu_ns_total) p
        gcdetails_elapsed_max_ns <- (# peek GCExtraGen, elapsed_ns_max) p
        gcdetails_elapsed_total_ns <- (# peek GCExtraGen, elapsed_ns_total) p
        gcdetails_sync_elapsed_max_ns <- (# peek GCExtraGen, sync_elapsed_ns_max) p
        gcdetails_sync_elapsed_total_ns <- (# peek GCExtraGen, sync_elapsed_ns_total) p
        return GCExtraGen{..}

type RtsTime = Int64

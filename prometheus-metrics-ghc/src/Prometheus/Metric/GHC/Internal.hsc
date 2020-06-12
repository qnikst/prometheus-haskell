-- | Internal datastructure for additional data.
-- This structure mostly copy 'RTStats' structure from base.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Prometheus.Metric.GHC.Internal
  ( getExtraStats
  , GCExtra(..)
  ) where

import Data.Int
import Data.Word
import GHC.Generics (Generic)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

#include "gc_extra.h"

-- | Populate structure.
foreign import ccall "populate_gc_extra" populate_gc_extra :: Ptr () -> IO ()

-- | Extra data.
data GCExtra = GCExtra {
    -- | The generation number of this GC
    gcdetails_gen :: Word32
    -- | Number of threads used in this GC
  , gcdetails_allocated_bytes :: Word64
    -- | Total amount of live data in the heap (incliudes large + compact data).
    -- Updated after every GC. Data in uncollected generations (in minor GCs)
    -- are considered live.
  , gcdetails_live_bytes :: Word64
    -- | Total amount of live data in large objects
  , gcdetails_large_objects_bytes :: Word64
    -- | Total amount of live data in compact regions
  , gcdetails_compact_bytes :: Word64
    -- | Total amount of slop (wasted memory)
  , gcdetails_slop_bytes :: Word64
    -- | Total amount of memory in use by the RTS
  , gcdetails_mem_in_use_bytes :: Word64
    -- | Total amount of data copied during this GC
  , gcdetails_copied_bytes :: Word64
    -- | In parallel GC, the max amount of data copied by any one thread.
    -- Deprecated.
  , gcdetails_par_max_copied_bytes :: Word64
    -- | In parallel GC, the amount of balanced data copied by all threads
  , gcdetails_par_balanced_copied_bytes :: Word64
    -- | The time elapsed during synchronisation before GC
  , gcdetails_sync_elapsed_ns :: RtsTime
    -- | The CPU time used during GC itself
  , gcdetails_cpu_ns :: RtsTime
    -- | The time elapsed during GC itself
  , gcdetails_elapsed_ns :: RtsTime
  , total_sync_elapsed_ns :: RtsTime -- Sum of CPU time elapsed during synchronization before GC.
  } deriving ( Read
             , Show
             , Generic
             )

getExtraStats :: IO [GCExtra]
getExtraStats = do
  allocaBytes (2 * #size GCExtra) $ \p -> do
    populate_gc_extra p
    gc0 <- fetch 0 p
    gc1 <- fetch 1 (p `plusPtr` #size GCExtra)
    return [gc0, gc1]
  where
    fetch gcdetails_gen p = do
      gcdetails_allocated_bytes <- (# peek GCExtra, allocated_bytes) p
      gcdetails_live_bytes <- (# peek GCExtra, live_bytes) p
      gcdetails_large_objects_bytes <-
        (# peek GCExtra, large_objects_bytes) p
      gcdetails_compact_bytes <- (# peek GCExtra, compact_bytes) p
      gcdetails_slop_bytes <- (# peek GCExtra, slop_bytes) p
      gcdetails_mem_in_use_bytes <- (# peek GCExtra, mem_in_use_bytes) p
      gcdetails_copied_bytes <- (# peek GCExtra, copied_bytes) p
      gcdetails_par_max_copied_bytes <-
        (# peek GCExtra, par_max_copied_bytes) p
      gcdetails_par_balanced_copied_bytes <-
        (# peek GCExtra, par_balanced_copied_bytes) p
      gcdetails_sync_elapsed_ns <- (# peek GCExtra, sync_elapsed_ns) p
      gcdetails_cpu_ns <- (# peek GCExtra, cpu_ns) p
      gcdetails_elapsed_ns <- (# peek GCExtra, elapsed_ns) p
      total_sync_elapsed_ns <- (# peek GCExtra, elapsed_ns) p
      return GCExtra{..}

type RtsTime = Int64

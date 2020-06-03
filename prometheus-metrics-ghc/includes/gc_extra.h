#ifndef _GC_EXTRA_H
#define _GC_EXTRA_H

#include "Rts.h"
#include <stdint.h>

// maximum generation available.
uint32_t total_gens = 2;

// Clear extra statistics entry.
void extra_gc_stats_clear();

//
// Extra stats that are not available normally
// slightly modified version on GCGetails from RTS.
//
typedef struct GCExtra_ {
  uint64_t allocated_bytes;
  // Total amount of live data in the heap (incliudes large + compact data).
  // Updated after every GC. Data in uncollected generations (in minor GCs)
  // are considered live.
  uint64_t live_bytes;
  // Total amount of live data in large objects
  uint64_t large_objects_bytes;
  // Total amount of live data in compact regions
  uint64_t compact_bytes;
  // Total amount of slop (wasted memory)
  uint64_t slop_bytes;
  // Total amount of memory in use by the RTS
  uint64_t mem_in_use_bytes;
  // Total amount of data copied during this GC
  uint64_t copied_bytes;
  // In parallel GC, the max amount of data copied by any one thread
  uint64_t par_max_copied_bytes;
  // In parallel GC, the amount of balanced data copied by all threads
  uint64_t par_balanced_copied_bytes;
  // The time elapsed during synchronisation before GC
  Time sync_elapsed_ns; // The CPU time used during GC itself
  Time cpu_ns; // The time elapsed during GC itself
  Time elapsed_ns; //
  Time total_sync_elapsed_ns; // Sum of CPU time elapsed during synchronization before GC.
} GCExtra;


#endif

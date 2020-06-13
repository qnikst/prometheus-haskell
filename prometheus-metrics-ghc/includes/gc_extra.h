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
// This is information that is relevant exactly per concrete generation
//
typedef struct GCExtraGen_ {
  uint64_t count;
  // Total amount of bytes allocated in the heap during concrete gen.
  uint64_t allocated_bytes_total;
  uint64_t allocated_bytes_max;
  // Total amount of data copied during this GC
  uint64_t copied_bytes_total;
  uint64_t copied_bytes_max;
  // In parallel GC, the max amount of data copied by any one thread
  uint64_t par_max_copied_bytes;
  // In parallel GC, the amount of balanced data copied by all threads
  uint64_t par_balanced_copied_bytes;
  // The time elapsed during GC itself
  Time cpu_ns_total;
  Time cpu_ns_max;
  // Wall clock time spend in GC.
  Time elapsed_ns_max; 
  Time elapsed_ns_total; 
  // The time elapsed during synchronisation before GC
  Time sync_elapsed_ns_max;
  Time sync_elapsed_ns_total;
} GCExtraGen;

typedef struct GCExtra_ {
  // Total amount of live data in the heap (incliudes large + compact data).
  // Updated after every GC. Data in uncollected generations (in minor GCs)
  // are considered live.
  uint64_t live_bytes_max;
  uint64_t live_bytes_current;
  // total amount of live data in large objects
  uint64_t large_objects_bytes_max;
  uint64_t large_objects_bytes_current;
  // total amount of live data in compact regions
  uint64_t compact_bytes_max;
  uint64_t compact_bytes_current;
  // total amount of slop (wasted memory)
  uint64_t slop_bytes_max;
  uint64_t slop_bytes_current;
  // total amount of memory in use by the rts
  uint64_t mem_in_use_bytes_max;
  uint64_t mem_in_use_bytes_current;
} GCExtra;


#endif

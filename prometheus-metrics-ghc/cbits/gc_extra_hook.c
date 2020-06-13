#include "Rts.h"
#include "gc_extra.h"
#include "memory.h"

#define max(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

// Include rts configuration that we need to update by our hoooks.
extern RtsConfig rtsConfig;

// Current extra GC info data.
//
// __N.B.__ Currently we have hardcoded data, this is actually very bad,
// it would be better if we can support any number of generation passed
// with -g.
struct GCExtraGen_ info_gen[2];

// Extra information about memory usage.
struct GCExtra_ info_extra[1];

// Old GC, hook to call after our one. We don't want to loose
// hooks in case someone else has used them.
void  (*old_hook)(const struct GCDetails_ *stats) = NULL;

// Hooks that is called after each GC. In the current hook we store maximum
// for all the values per each generation. And in addition we provide a hook
// that allows to clear current maximum. This way we can introduce "windows"
// for values.
//
// Note. Another option may be introducing rolling or even HDR percentilles
// this way we still can get some insite on the statistics but it comes at a
// cost, we need some experimenting with that.
void extra_gc_hook(const struct GCDetails_ *stats) {
  struct GCExtraGen_ * info = &info_gen[stats->gen];

  info->count += 1;
  info->allocated_bytes_max = max(info->allocated_bytes_max, stats->allocated_bytes);
  info->allocated_bytes_total += stats->allocated_bytes;

  info->copied_bytes_max = max(info->copied_bytes_max, stats->copied_bytes);
  info->copied_bytes_total += stats->copied_bytes;

  info->par_max_copied_bytes = max(info->par_max_copied_bytes, stats->par_max_copied_bytes);
  info->par_balanced_copied_bytes= max(info->par_balanced_copied_bytes, stats->par_balanced_copied_bytes);

  info->elapsed_ns_max = max(info->elapsed_ns_max, stats->elapsed_ns);
  info->elapsed_ns_total += stats->elapsed_ns;

  info->cpu_ns_max = max(info->cpu_ns_max, stats->cpu_ns);
  info->cpu_ns_total += stats->cpu_ns;

  info->sync_elapsed_ns_total = max(info->sync_elapsed_ns_total, stats->sync_elapsed_ns);
  info->sync_elapsed_ns_total += stats->sync_elapsed_ns;

  info_extra->live_bytes_max = max(info_extra->live_bytes_max, stats->live_bytes);
  info_extra->live_bytes_current = stats->live_bytes;

  info_extra->large_objects_bytes_max = max(info_extra->large_objects_bytes_max, stats->large_objects_bytes);
  info_extra->large_objects_bytes_current = stats->large_objects_bytes;

  info_extra->compact_bytes_max = max(info_extra->compact_bytes_max, stats->compact_bytes);
  info_extra->compact_bytes_current = stats->compact_bytes;

  info_extra->slop_bytes_max = max(info_extra->slop_bytes_max, stats->slop_bytes);
  info_extra->slop_bytes_current = stats->slop_bytes;

  info_extra->mem_in_use_bytes_max = max(info_extra->mem_in_use_bytes_max, stats->mem_in_use_bytes);
  info_extra->mem_in_use_bytes_current = stats->mem_in_use_bytes;

  if (old_hook) {
    old_hook(stats);
  }
}

// Load current data into the remote structures.
void populate_gc_extra(struct GCExtraGen_ targets[2], struct GCExtra_ target_extra[1]) {
   targets[0] = info_gen[0];
   targets[1] = info_gen[1];
   target_extra = info_extra;
}

// Clear runtime statistics. You may need this when you want to clear current window,
// and start a new one, that allows to get window based maximum.
void extra_gc_stats_clear() {
  for (int i=0;i<2;i++) {
    info_gen[i].allocated_bytes_max = 0;
    info_gen[i].copied_bytes_max = 0;
    info_gen[i].par_max_copied_bytes = 0;
    info_gen[i].par_balanced_copied_bytes = 0;
    info_gen[i].sync_elapsed_ns_max = 0;
    info_gen[i].cpu_ns_max = 0;
    info_gen[i].elapsed_ns_max = 0;
  }

  info_extra->live_bytes_max = info_extra->live_bytes_current;
  info_extra->large_objects_bytes_max = info_extra->large_objects_bytes_current;
  info_extra->compact_bytes_max = info_extra->compact_bytes_current;
  info_extra->slop_bytes_max = info_extra->slop_bytes_current;
  info_extra->mem_in_use_bytes_max = info_extra->mem_in_use_bytes_current;
}

// Set new gc hook, old one is preserved.
void set_extra_gc_hook() {
  old_hook = rtsConfig.gcDoneHook;
  rtsConfig.gcDoneHook = extra_gc_hook;
  memset(info_extra, 0, sizeof(info_extra[0]));
  memset(info_gen, 0, 2*sizeof(info_gen[0]));
}

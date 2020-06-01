#include "Rts.h"
#include "gc_extra.h"

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
struct GCExtra_ infos[2];

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
  struct GCExtra_ * info = &infos[stats->gen];
  info->allocated_bytes = max(info->allocated_bytes, stats->allocated_bytes);
  info->live_bytes = max(info->live_bytes, stats->live_bytes);
  info->large_objects_bytes = max(info->large_objects_bytes, stats->large_objects_bytes);
  info->compact_bytes = max(info->compact_bytes, stats->compact_bytes);
  info->slop_bytes = max(info->compact_bytes, stats->slop_bytes);
  info->mem_in_use_bytes = max(info->mem_in_use_bytes, stats->mem_in_use_bytes);
  info->copied_bytes = max(info->copied_bytes, stats->copied_bytes);
  info->par_max_copied_bytes = max(info->par_max_copied_bytes, stats->par_max_copied_bytes);
  info->par_balanced_copied_bytes= max(info->par_balanced_copied_bytes, stats->par_balanced_copied_bytes);
  info->sync_elapsed_ns= max(info->sync_elapsed_ns, stats->sync_elapsed_ns);
  info->cpu_ns= max(info->cpu_ns, stats->cpu_ns);
  info->elapsed_ns = max(info->elapsed_ns, stats->elapsed_ns);
  info->total_sync_elapsed_ns += stats->sync_elapsed_ns;
  if (old_hook) {
    old_hook(stats);
  }
}

// Load current data into the remote structures.
void populate_gc_extra(struct GCExtra_ *targets) {
   targets[0] = infos[0];
   targets[1] = infos[1];
}

// Clear runtime statistics. You may need this when you want to clear current window,
// and start a new one, that allows to get window based maximum.
void extra_gc_stats_clear() {
  for (int i=0;i<2;i++) {
    infos[i].allocated_bytes = 0;
    infos[i].live_bytes = 0;
    infos[i].large_objects_bytes = 0;
    infos[i].compact_bytes = 0;
    infos[i].slop_bytes = 0;
    infos[i].mem_in_use_bytes = 0;
    infos[i].copied_bytes = 0;
    infos[i].par_max_copied_bytes = 0;
    infos[i].par_balanced_copied_bytes = 0;
    infos[i].sync_elapsed_ns = 0;
    infos[i].cpu_ns= 0;
    infos[i].elapsed_ns = 0;
  }
}

// Set new gc hook, old one is preserved.
void set_extra_gc_hook() {
  old_hook = rtsConfig.gcDoneHook;
  rtsConfig.gcDoneHook = extra_gc_hook;
  extra_gc_stats_clear();
}

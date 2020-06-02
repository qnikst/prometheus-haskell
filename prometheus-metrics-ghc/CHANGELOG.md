# Change Log

## 1.1.0

- Introduce metrics gathering via a C hook.
- Gather maximum in a window values for gcdetails (see Readme).
- Drop old GHC support.
- Simplify internals.

## 1.0.1

- Allow GHC metrics to be labeled via `ghcMetricsWithLabels`.

## 1.0.0

- Supports prometheus-client-1.0.0

## 0.3.0

## 0.2.0

## 0.1.1

- Metrics that are always incrementing are now counters instead of gauges.
- Metric names have changed to be more inline with prometheus guidelines.

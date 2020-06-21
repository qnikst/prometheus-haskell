{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Prometheus
import qualified Prometheus.Metric.IntCounter as I
import qualified Prometheus.Metric.IntGauge as I


main :: IO ()
main =
  defaultMain
    [ benchIntCounter
    , benchIntGauge
    ]

withMetric m =
  envWithCleanup
    (register m)
    (const unregisterAll)

withIntCounter =
  withMetric (counter (Info "a" "b"))

benchIntCounter =
  withIntCounter $ \counter ->
    bgroup "IntCounter"
      [ benchIncIntCounter counter
      , benchAddIntCounter counter
      ]

benchIncIntCounter c =
  bench "incCounter" $ whnfIO (incCounter c)

benchAddIntCounter c =
  bench "addCounter" $ whnfIO (addCounter c 40)

withIntGauge =
  withMetric (gauge (Info "a" "b"))

benchIntGauge =
  withIntGauge $ \gauge -> 
    bgroup "IntGauge"
      [ benchIncGauge gauge
      , benchAddGauge gauge
      , benchSubGauge gauge
      , benchGetGauge gauge
      ]

benchIncGauge testGauge =
  bench "incGauge" $ whnfIO (incGauge testGauge)

benchAddGauge testGauge =
  bench "addGauge" $ whnfIO (addGauge testGauge 50)

benchSubGauge testGauge =
  bench "subGauge" $ whnfIO (subGauge testGauge 50)

benchGetGauge testGauge =
  bench "getGauge" $ whnfIO (getGauge testGauge)


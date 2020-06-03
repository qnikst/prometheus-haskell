#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , criterion
             , typed-process
-}

import Criterion.Main
import System.Process.Typed



main :: IO ()
main = do
  defaultMain
    [ bgroup "10"
       [ bench "no-metrics"  $ nfIO $ runProcess_  $ setStdout nullStream $ proc "./Main" ["10"]
       , bench "metrics"  $ nfIO $ runProcess_  $ setStdout nullStream $ proc "./Main" ["10","metrics"]
       ]
    --, bgroup "100"
    --   [ bench "no-metrics"  $ nfIO $ runProcess_  $ setStdout nullStream $ proc "./Main" ["100"]
    --   , bench "metrics"  $ nfIO $ runProcess_  $ setStdout nullStream $ proc "./Main" ["100","metrics"]
    --   ]
    ]

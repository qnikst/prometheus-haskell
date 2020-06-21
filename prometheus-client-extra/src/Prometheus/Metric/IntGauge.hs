{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
-- | Atomic integral value gauge.
module Prometheus.Metric.IntGauge
  ( IntGauge
  , gauge
  , incGauge
  , decGauge
  , addGauge
  , subGauge
  , setGauge
  , getGauge
  ) where

import Prometheus hiding (gauge, incGauge, decGauge, addGauge, subGauge, setGauge, getGauge)

import GHC.Base  hiding ((==#))

import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BS

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

-- | Efficient gauge for the integral value.
-- Uses atomic non-blocking operations.
data IntGauge = MkGauge (MutableByteArray# RealWorld)

-- | Create a new gauge metric with a given name and help string.
gauge :: Info -> Metric IntGauge
gauge info = Metric $ do
  atomic <- IO $ \s ->
    case newByteArray# size s of { (# s', arr #) -> (# s', MkGauge arr #) }
  return (atomic, collectGauge info atomic)
  where !(I# size) = SIZEOF_HSINT

-- | Adds a value to a gauge metric.
--
-- Value is added as atomic processor operation
addGauge :: MonadIO m => IntGauge -> Int -> m ()
{-# INLINE addGauge #-}
addGauge (MkGauge mba#) !(I# incr#) = liftIO $ IO $ \ s1# ->
  case fetchAddIntArray# mba# 0# incr# s1# of
    (# s2#, _ #) -> (# s2#, () #)

-- | Subtracts a value from a gauge metric.
--
-- Value is added as atomic processor operation
subGauge :: MonadIO m => IntGauge -> Int -> m ()
{-# INLINE subGauge #-}
subGauge (MkGauge mba#) !(I# incr#) = liftIO $ IO $ \ s1# ->
  case fetchSubIntArray# mba# 0# incr# s1# of
    (# s2#, _ #) -> (# s2#, () #)

-- | Increments a gauge metric by 1.
incGauge :: MonadIO m => IntGauge -> m ()
{-# INLINE incGauge #-}
incGauge g = addGauge g 1

-- | Decrements a gauge metric by 1.
decGauge :: MonadIO m => IntGauge -> m ()
{-# INLINE decGauge #-}
decGauge g = subGauge g 1

-- | Sets a gauge metric to a specific value.
--
-- This function doesn't set memory barriers.
setGauge :: MonadIO m => IntGauge -> Int -> m ()
setGauge (MkGauge arr#) (I# i#) = liftIO $ IO $ \s ->
  case writeIntArray# arr# 0# i# s of { s' -> (# s', () #) }

-- | Retrieves the current value of a gauge metric.
-- (non-atomic, not implying any memory barriers).
getGauge :: MonadIO m => IntGauge -> m Int
getGauge (MkGauge arr#) = liftIO $ IO \s0# -> 
  case readIntArray#  arr# 0# s0# of
   { (# s', i #) -> (# s', I# i #) }

collectGauge :: Info -> IntGauge -> IO [SampleGroup]
collectGauge info c = do
    value <- getGauge c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info GaugeType [sample]]

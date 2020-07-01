{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
-- | Atomic integral value gauge with a window.
module Prometheus.Metric.WindowGauge
  ( WinGauge
  , gauge
  , incGauge
  , decGauge
  , addGauge
  , subGauge
  , setGauge
  , getGauge
  , refreshGauge
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
data WinGauge = MkGauge (MutableByteArray# RealWorld)

-- | Create a new gauge metric with a given name and help string.
gauge :: Info -> Metric WinGauge
gauge info = Metric $ do
  atomic <- IO $ \s0# ->
    case newByteArray# (2# *# size) s0# of
      (# s1#, arr# #) ->
         case atomicWriteIntArray# arr# 0# 0# s1# of
           s2# -> case atomicWriteIntArray# arr# 1# 0# s2# of
             s3# -> (# s3# , MkGauge arr# #)
  return (atomic, collectGauge info atomic)
  where !(I# size) = SIZEOF_HSINT

-- | Adds a value to a gauge metric.
--
-- Value is added as atomic processor operation
addGauge :: MonadIO m => WinGauge -> Int -> m ()
{-# INLINE addGauge #-}
addGauge (MkGauge mba#) !(I# incr#) = liftIO $ IO $ \ s1# ->
  case fetchAddIntArray# mba# 1# incr# s1# of
    (# s2#, i0# #) -> 
      case atomicReadIntArray# mba# 0# s2# of
        (# s3#, i1# #) -> case ( i0# +# incr# ) >=# i1# of
           1# -> case atomicWriteIntArray# mba# 0# (i0# +# incr# ) s3# of
              s4# -> (# s4#, () #)
           _  -> (# s3#, () #)

-- | Subtracts a value from a gauge metric.
--
-- Value is added as atomic processor operation
subGauge :: MonadIO m => WinGauge -> Int -> m ()
{-# INLINE subGauge #-}
subGauge (MkGauge mba#) !(I# incr#) = liftIO $ IO $ \ s1# ->
  case fetchSubIntArray# mba# 1# incr# s1# of
    (# s2#, _ #) -> (# s2#, () #)

-- | Increments a gauge metric by 1.
incGauge :: MonadIO m => WinGauge -> m ()
{-# INLINE incGauge #-}
incGauge g = addGauge g 1

-- | Decrements a gauge metric by 1.
decGauge :: MonadIO m => WinGauge -> m ()
{-# INLINE decGauge #-}
decGauge g = subGauge g 1

-- | Sets a gauge metric to a specific value.
--
-- This function doesn't set memory barriers.
setGauge :: MonadIO m => WinGauge -> Int -> m ()
setGauge (MkGauge arr#) (I# i#) = liftIO $ IO $ \s ->
  case atomicReadIntArray# arr# 0# s of
   (# s1#, i0# #) -> 
     case atomicWriteIntArray# arr# 0# (if isTrue# (i# >=# i0#) then i# else i0#) s1# of
       s2# -> case atomicWriteIntArray# arr# 1# i# s2# of
          s3# -> (# s3#, () #)

-- | Retrieves the current value of a gauge metric.
-- (non-atomic, not implying any memory barriers).
getGauge :: MonadIO m => WinGauge -> m Int
getGauge (MkGauge arr#) = liftIO $ IO \s0# -> 
  case readIntArray#  arr# 0# s0# of
   { (# s', i #) -> (# s', I# i #) }

refreshGauge :: MonadIO m => WinGauge -> m Int
refreshGauge (MkGauge arr#) = liftIO $ IO \s0# ->
  case readIntArray#  arr# 1# s0# of
    (# s1#, i# #) -> case readIntArray# arr# 0# s1# of
      (# s2#, j# #) -> case atomicWriteIntArray# arr# 0# i# s2# of 
         s3# -> (# s3#, I# j# #)

collectGauge :: Info -> WinGauge -> IO [SampleGroup]
collectGauge info c = do
    value <- refreshGauge c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info GaugeType [sample]]

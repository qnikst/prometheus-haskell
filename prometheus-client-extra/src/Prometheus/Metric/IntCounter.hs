-- | 
-- Counter for intergral values. This counter is much more effient comparing to
-- the default 'Counter' that keeps floating point value. This counter uses
-- atomic operations and works in constant memory.
--
-- Consider using it in case if your keep intergral value.
-- 
-- Api mimics 'Counter' API, excluding operations that are not applicable and is intended
-- to be imported qualified.
-- 
module Prometheus.Metric.IntCounter
  ( IntCounter
  , counter
  , incCounter
  , addCounter
  , getCounter
  ) where

import Control.Monad.IO.Class
import Data.Atomics.Counter
import qualified Data.ByteString.UTF8 as BS
import Prometheus hiding (counter, getCounter, incCounter, addCounter, getCounter)

-- | Integral counter that can store values of 'Int' type. Values
-- are not checked for the overflow.
newtype IntCounter = IntCounter AtomicCounter

-- | Creates a new counter with a given name and help string.
--
-- >>> c <- register $ counter (Info "test" "Just test counter")
counter :: Info -> Metric IntCounter
counter info = Metric $ do
  atomic <- newCounter 0
  return (IntCounter atomic, collectCounter info atomic)

-- | Increment counter on 1.
--
-- >>> c <- register $ counter (Info "test" "Just test counter")
-- >>> incCounter c
-- >>> getCounter c
-- 1
incCounter :: MonadIO m => IntCounter -> m ()
incCounter (IntCounter atomic) = liftIO $ incrCounter_ 1 atomic
{-# SPECIALIZE incCounter :: IntCounter -> IO () #-}

-- | Add a positive value to the counter.
--
-- Returns @Bool@ in case if value was added (was positive)
--
-- >>> c <- register $ counter (Info "test" "Just test counter")
-- >>> addCounter 1 c
-- True
-- >>> addCounter (-1) c
-- False
-- 
-- __N.B.__ This method doesn't check arithmetic overflow.
addCounter :: MonadIO m => Int -> IntCounter -> m Bool
addCounter x (IntCounter atomic)
  | x < 0 = pure False
  | otherwise = liftIO $ incrCounter_ x atomic >> pure True
{-# SPECIALIZE addCounter :: Int -> IntCounter -> IO Bool #-}

-- | Reads current value in a counter.
getCounter :: MonadIO m => IntCounter -> m Int
getCounter (IntCounter atomic) = liftIO $ readCounter atomic
{-# SPECIALIZE getCounter :: IntCounter -> IO Int #-}


-- | Collects metrics in a form that in understandable to the
-- prometheus-client.
collectCounter :: Info -> AtomicCounter -> IO [SampleGroup]
collectCounter info atomic = do
  value <- readCounter atomic
  let sample = Sample (metricName info) [] (BS.fromString $ show value) -- XXX: fix it!
  pure [SampleGroup info CounterType [sample]]

-- $setup
-- >>> :set -XOverloadedStrings 

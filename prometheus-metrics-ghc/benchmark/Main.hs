import Control.Monad
import Prometheus
import Prometheus.Metric.GHC
import Data.Foldable
import Data.Maybe
import Text.Read
import System.Environment

fib 0 = 0
fib 1 = 1
fib x = fib (x-2) + fib (x-1)

main :: IO ()
main = do
  (n:xs) <- getArgs
  let num = fromMaybe 1 $ readMaybe n
  when ("metrics" `elem` xs) $ do
    _ <- register ghcMetrics
    pure ()
  replicateM_ num $ print $ fib 39

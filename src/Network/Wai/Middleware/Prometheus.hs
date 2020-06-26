{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Network.Wai.Middleware.Prometheus
    ( ApplicationMetrics
    , applicationMetrics
    , countStatusCode
    , observeDuration
    , instrumentApplication
    ) where

import           Control.Monad.IO.Class                         (MonadIO)
import           Data.Map.Strict                                (Map, fromList)
import qualified Data.Map.Strict                                as Map
import           Data.Text                                      (pack)
import           Network.HTTP.Types                             (Status (..))
import           Network.Wai                                    (Middleware,
                                                                 responseStatus)
import           System.Clock                                   (Clock (Monotonic),
                                                                 TimeSpec (..),
                                                                 diffTimeSpec,
                                                                 getTime,
                                                                 toNanoSecs)
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT,
                                                                 registerCounter,
                                                                 registerHistogram)
import           System.Metrics.Prometheus.Metric.Counter       (Counter, inc)
import           System.Metrics.Prometheus.Metric.Histogram     (Histogram,
                                                                 observe)
import           System.Metrics.Prometheus.MetricId             (Labels (..))


data ApplicationMetrics = ApplicationMetrics
    { statusCodeMetrics :: Map Int Counter
    , durationMetrics   :: Histogram
    }


-- | Increment the count for a specific status code, by number
countStatusCode :: ApplicationMetrics -> Int -> IO ()
countStatusCode ms s = mapM_ inc . Map.lookup s $ statusCodeMetrics ms


-- | Add a request duration observation
observeDuration :: ApplicationMetrics -> Double -> IO ()
observeDuration ms i = observe i $ durationMetrics ms


-- | Set up the metrics for HTTP response codes.  Use labels to identify your particular application.  The label
-- @http_response_code@ is reserved.
applicationMetrics :: MonadIO m => Labels -> RegistryT m ApplicationMetrics
applicationMetrics ls =
    ApplicationMetrics . fromList <$> traverse codeCounter codes <*> hist
  where
    codeCounter i = (i, ) <$> registerCounter "http_requests_total" (mkLabels i)
    mkLabels i    = Labels $ fromList [("http_response_code", (pack . show) i)] <> unLabels ls

    codes = [100 .. 103]
         <> [200 .. 208] <> [226]
         <> [300 .. 308]
         <> [400 .. 418] <> [421 .. 426] <> [428, 429, 431, 451]
         <> [500 .. 508] <> [510, 511]

    hist           = registerHistogram "http_request_duration_milliseconds" ls durationBounds
    durationBounds = [1 .. 20] <> [30, 40 .. 200] <> [300, 400 .. 900] <> [1000, 2000 .. 10000]


-- | This middleware adds response code tracking for the application, aggregating across all requests
instrumentApplication :: ApplicationMetrics -> Middleware
instrumentApplication ms app req respond = do
    t0 <- getTime Monotonic
    app req $ \r -> do
        t1 <- getTime Monotonic
        countStatusCode ms (statusCode $ responseStatus r)
        observeDuration ms $ diffTimeMS t0 t1
        respond r


diffTimeMS :: TimeSpec -> TimeSpec -> Double
diffTimeMS t0 t1 = toMS $ t1 `diffTimeSpec` t0
  where
    toMS = fromIntegral . (`quot` oneMillion) . toNanoSecs
    oneMillion = 1000000

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Network.Wai.Middleware.Prometheus
    ( ApplicationMetrics
    , applicationMetrics
    , countStatusCode
    , observeDuration
    , instrumentApplication
    ) where

import           Control.Monad                                  (unless)
import           Control.Monad.IO.Class                         (MonadIO)
import qualified Data.CaseInsensitive                           as CI
import           Data.Map.Strict                                (Map, fromList)
import qualified Data.Map.Strict                                as Map
import           Data.Text                                      (pack)
import           Network.HTTP.Types                             (Status (..))
import           Network.Wai                                    (Middleware,
                                                                 Request,
                                                                 requestHeaders,
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


-- | Add a request duration observation in ms
observeDuration :: ApplicationMetrics -> Double -> IO ()
observeDuration ms i = observe i $ durationMetrics ms


-- | Set up the metrics for HTTP response codes and request handling durations.  We identify the response code counters
-- by @http_requests_total@ with codes labeled by @http_response_code@.  We identify the duration histogram by
-- @http_request_duration_milliseconds@ Use labels to identify your particular application.
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


-- | Is this a WebSocket request?  Returns 'True' if it is.
--
-- This is taken from "Network.Wai.Handler.WebSockets".
isWebSocketsReq :: Request -> Bool
isWebSocketsReq req =
    fmap CI.mk (lookup "upgrade" $ requestHeaders req) == Just "websocket"


-- | This middleware adds response code tracking and request duration statistics for the application, aggregating across all requests
instrumentApplication :: ApplicationMetrics -> Middleware
instrumentApplication ms app req respond = do
    t0 <- getTime Monotonic
    app req $ \r -> do
        t1 <- getTime Monotonic
        -- We don't want to count WebSocket requests, since they do not have a
        -- response status code.
        --
        -- TODO: As far as I understand, this is not quite correct.  Here, we
        -- are testing whether a _request_ is a WebSocket request, but a web server
        -- may send a normal HTTP response to a WebSocket request.  We should be
        -- testing the _response_ is a WebSocket response.  However, it doesn't
        -- look like Wai/Warp has a good way to do this easily.
        --
        -- You can't easily call responseStatus on this response, because Wai ends
        -- up using a 5XX status code that has been hard-coded as a fall-back
        -- response.
        unless (isWebSocketsReq req) $
          countStatusCode ms (statusCode $ responseStatus r)
        observeDuration ms $ diffTimeMS t0 t1
        respond r


diffTimeMS :: TimeSpec -> TimeSpec -> Double
diffTimeMS t0 t1 = toMS $ t1 `diffTimeSpec` t0
  where
    toMS = fromIntegral . (`quot` oneMillion) . toNanoSecs
    oneMillion = 1000000

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async                       (async)
import           Control.Monad.IO.Class                         (liftIO)
import           Network.HTTP.Types.Status                      (status200,
                                                                 status404)
import           Network.Wai                                    (pathInfo,
                                                                 responseLBS)
import           Network.Wai.Handler.Warp                       (run)
import           System.Metrics.Prometheus.Concurrent.RegistryT (runRegistryT)
import           System.Metrics.Prometheus.Http.Scrape          (serveMetricsT)
import           System.Metrics.Prometheus.MetricId             (fromList)

import           Network.Wai.Middleware.Prometheus              (applicationMetrics,
                                                                 instrumentApplication)


main :: IO ()
main = runRegistryT $ do
    ms <- applicationMetrics $ fromList [("app", "example_server")]
    let webserver = run 8080 . instrumentApplication ms $ \req respond ->
            respond (makeResponse req)
    _ <- liftIO $ async webserver
    serveMetricsT 8081 []
  where
    success = responseLBS status200 [] "ok"
    failure = responseLBS status404 [] mempty
    makeResponse req
        | pathInfo req == ["test"] = success
        | otherwise = failure

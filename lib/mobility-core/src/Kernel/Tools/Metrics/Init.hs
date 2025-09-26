{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.Tools.Metrics.Init where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Ratio ((%))
import qualified Data.Text as DT
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import EulerHS.Prelude as E hiding (decodeUtf8)
import Kernel.Prelude (lookup, (!!))
import Kernel.Tools.Metrics.CoreMetrics.Types hiding (requestLatency)
import Kernel.Utils.Monitoring.Prometheus.Servant
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, Request (..))
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Prometheus (metricsApp)
import Prometheus as P
import qualified Prometheus as Prom
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc
import System.Clock (Clock (..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)

{-# NOINLINE requestLatency #-}
requestLatency :: Prom.Vector Prom.Label3 Prom.Histogram
requestLatency =
  Prom.unsafeRegister $
    Prom.vector ("handler", "method", "status_code") $
      Prom.histogram info Prom.defaultBuckets
  where
    info =
      Prom.Info
        "http_request_duration_seconds"
        "The HTTP request latencies in seconds."

{-# NOINLINE requestLatencyWithVersionLabel #-}
requestLatencyWithVersionLabel :: Prom.Vector Prom.Label6 Prom.Histogram
requestLatencyWithVersionLabel =
  Prom.unsafeRegister $
    Prom.vector ("handler", "method", "status_code", "version", "x_client_version", "x_bundle_version") $
      Prom.histogram info Prom.defaultBuckets
  where
    info =
      Prom.Info
        "http_request_duration_seconds"
        "The HTTP request latencies in seconds."

serve :: Int -> IO ()
serve port = do
  _ <- register ghcMetrics
  _ <- register procMetrics
  putStrLn @String $ "Prometheus server started at port " <> show port
  _ <- forkIO $ W.run port (metricsApp Nothing)
  return ()

addServantInfo ::
  SanitizedUrl a =>
  DeploymentVersion ->
  Proxy a ->
  Application ->
  Application
addServantInfo version proxy app request respond =
  let fullpath = DT.intercalate "/" (pathInfo request)
   in instrumentHandlerValueWithVersionLabel version.getDeploymentVersion (\req -> "/" <> fromMaybe fullpath (getSanitizedUrl proxy req)) app request respond

instrumentHandlerValueWithVersionLabel ::
  -- | version label
  Text ->
  -- | The function used to derive the "handler" value in Prometheus
  (Wai.Request -> Text) ->
  -- | The app to instrument
  Wai.Application ->
  -- | The instrumented app
  Wai.Application
instrumentHandlerValueWithVersionLabel versionLabel = instrumentHandlerValueWithFilter (Just versionLabel) Just

-- | A more flexible variant of 'instrumentHandlerValue'.  The filter can change some
-- responses, or drop others entirely.
instrumentHandlerValueWithFilter ::
  -- | version label
  Maybe Text ->
  -- | Response filter
  (Wai.Response -> Maybe Wai.Response) ->
  -- | The function used to derive the "handler" value in Prometheus
  (Wai.Request -> Text) ->
  -- | The app to instrument
  Wai.Application ->
  -- | The instrumented app
  Wai.Application
instrumentHandlerValueWithFilter mbVersionLabel resFilter f app req respond = do
  start <- getTime Monotonic
  app req $ \res -> do
    case resFilter res of
      Nothing -> return ()
      Just res' -> do
        end <- getTime Monotonic
        let method = Just $ decodeUtf8 (Wai.requestMethod req)
        let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res')))
        let requiredHeaders = map (T.pack . show <$>) (flip lookup (Wai.requestHeaders req) . stringToCI <$> ["x-client-version", "x-bundle-version"])
        observeSeconds mbVersionLabel (f req) method status start end requiredHeaders
    respond res

stringToCI :: String -> CI ByteString
stringToCI = CI.mk . fromString

observeSeconds ::
  -- | version label
  Maybe Text ->
  -- | handler label
  Text ->
  -- | method
  Maybe Text ->
  -- | status
  Maybe Text ->
  -- | start time
  TimeSpec ->
  -- | end time
  TimeSpec ->
  -- | required headers
  [Maybe Text] ->
  IO ()
observeSeconds mbVersionLabel handler method status start end requiredHeaders = do
  let latency :: Double
      latency = fromRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
  case mbVersionLabel of
    Nothing -> do
      Prom.withLabel
        requestLatency
        (handler, fromMaybe "" method, fromMaybe "" status)
        (flip Prom.observe latency)
    Just versionLabel -> do
      Prom.withLabel
        requestLatencyWithVersionLabel
        (handler, fromMaybe "" method, fromMaybe "" status, versionLabel, fromMaybe "" (requiredHeaders !! 0), fromMaybe "" (requiredHeaders !! 1))
        (flip Prom.observe latency)

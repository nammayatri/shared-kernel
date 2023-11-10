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
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

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
import Data.Text as DT hiding (filter)
import EulerHS.Prelude as E
import Kernel.Tools.Metrics.CoreMetrics.Types
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
requestLatency = Prom.unsafeRegister $ Prom.vector ("handler", "method", "status_code")
                                     $ Prom.histogram info Prom.defaultBuckets
    where info = Prom.Info "http_request_duration_seconds"
                           "The HTTP request latencies in seconds."

{-# NOINLINE requestLatencyWithLabels #-}
requestLatencyWithLabels :: Prom.Vector Prom.Label5 Prom.Histogram
requestLatencyWithLabels = Prom.unsafeRegister $ Prom.vector ("handler", "method", "status_code", "version", "priority")
                                                     $ Prom.histogram info Prom.defaultBuckets
    where info = Prom.Info "http_request_duration_seconds"
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
  let mpath = getSanitizedUrl proxy request
      fullpath = DT.intercalate "/" (pathInfo request)
   in instrumentHandlerValueWithVersionLabel version.getDeploymentVersion (\_ -> "/" <> fromMaybe fullpath mpath) app request respond

instrumentHandlerValueWithLabels ::
     Text -- ^ version label
  -> Text
  -> (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValueWithLabels versionLabel priority = instrumentHandlerValueWithFilter (Just versionLabel) (Just priority) Just

-- | A more flexible variant of 'instrumentHandlerValue'.  The filter can change some
-- responses, or drop others entirely.
instrumentHandlerValueWithFilter ::
     Maybe Text -- ^ version label
  -> Maybe Text -- ^ priority label
  -> (Wai.Response -> Maybe Wai.Response) -- ^ Response filter
  -> (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValueWithFilter mbVersionLabel mbPriority resFilter f app req respond = do
  start <- getTime Monotonic
  app req $ \res -> do
    case resFilter res of
      Nothing -> return ()
      Just res' -> do
        end <- getTime Monotonic
        let method = Just $ decodeUtf8 (Wai.requestMethod req)
        let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res')))
        observeSeconds mbVersionLabel mbPriority (f req) method status start end
    respond res

stringToCI :: String -> CI ByteString
stringToCI = CI.mk . fromString

observeSeconds :: Maybe Text   -- ^ version label
               -> Maybe Text   -- ^ priority label 
               -> Text         -- ^ handler label
               -> Maybe Text   -- ^ method
               -> Maybe Text   -- ^ status
               -> TimeSpec     -- ^ start time
               -> TimeSpec     -- ^ end time
               -> IO ()
observeSeconds mbVersionLabel mbPriority handler method status start end = do
    let latency :: Double
        latency = fromRational $ toRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
    case (mbVersionLabel, mbPriority) of
      (Just versionLabel, Just priorityLabel)  -> do
        Prom.withLabel requestLatencyWithLabels
                       (handler, fromMaybe "" method, fromMaybe "" status, versionLabel, priorityLabel)
                       (flip Prom.observe latency)  
      _ -> do
        Prom.withLabel requestLatency
                       (handler, fromMaybe "" method, fromMaybe "" status)
                       (flip Prom.observe latency)                          


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

import Data.Text as DT hiding (filter)
import EulerHS.Prelude as E
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Utils.Monitoring.Prometheus.Servant
import Network.Wai (Application, Request (..))
import Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Prometheus
import Prometheus as P
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc

-- import Kernel.Utils.Logging (logDebug)

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
  PriorityLabel ->
  Proxy a ->
  Application ->
  Application
addServantInfo version priority proxy app request respond =
  let mpath = getSanitizedUrl proxy request
      fullpath = DT.intercalate "/" (pathInfo request)
      validateIsTheUrlPriorityOrNonPriority = fullpath `elem` priority.getCriticalPriorityLabelUrls
      prioirtyField =
        if validateIsTheUrlPriorityOrNonPriority
          then "Priority"
          else "NonPrioirty"
   in instrumentHandlerValueWithLabels version.getDeploymentVersion prioirtyField (\_ -> "/" <> fromMaybe fullpath mpath) app request respond

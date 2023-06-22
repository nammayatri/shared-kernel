{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Idfy.Auth where

import qualified Data.HashMap.Strict as HMS
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Verification.Idfy.Config
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.IOLogging
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http

verifyAuth ::
  ( EncFlow m r,
    HasField "isShuttingDown" r (TMVar ()),
    HasField "coreMetrics" r CoreMetricsContainer,
    HasField "loggerEnv" r LoggerEnv,
    Log m
  ) =>
  IdfyCfg ->
  Maybe Text ->
  m ()
verifyAuth cfg authSecret = do
  cfgSecret <- decrypt cfg.secret
  unless (authSecret == Just cfgSecret) $ throwError (InvalidRequest "INVALID_AUTHORIZATION_HEADER")

prepareIdfyHttpManager :: Int -> HashMap Text Http.ManagerSettings
prepareIdfyHttpManager timeout =
  HMS.singleton idfyHttpManagerKey $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

idfyHttpManagerKey :: Text
idfyHttpManagerKey = "idfy-http-manager"

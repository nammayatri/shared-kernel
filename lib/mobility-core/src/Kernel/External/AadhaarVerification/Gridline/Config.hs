{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.AadhaarVerification.Gridline.Config where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.Prelude
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http

data GridlineCfg = GridlineCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    authType :: Text,
    url :: BaseUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

prepareGridlineHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareGridlineHttpManager timeout =
  HMap.singleton (DT.pack gridlineHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

gridlineHttpManagerKey :: String
gridlineHttpManagerKey = "gridline-http-manager"

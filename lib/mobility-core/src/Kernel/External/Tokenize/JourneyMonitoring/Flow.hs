{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Tokenize.JourneyMonitoring.Flow where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DE
import EulerHS.Types (ManagerSelector (..), client)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Tokenize.JourneyMonitoring.Types as JourneyMonitoringTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type JourneyMonitoringTokenizeAPI =
  "WBJMONRestAPI" :> "user" :> "auth" :> "generateToken"
    :> BasicAuth "username-password" BasicAuthData
    :> Post '[JSON] JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse

tokenize ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r
  ) =>
  JourneyMonitoringTypes.JourneyMonitoringTokenizeConfig ->
  m JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse
tokenize JourneyMonitoringTypes.JourneyMonitoringTokenizeConfig {..} = do
  username_ <- decrypt username
  password_ <- decrypt password
  let authData = BasicAuthData (DE.encodeUtf8 $ username_) (DE.encodeUtf8 $ password_)
  callAPI' ((Just $ ManagerSelector $ DT.pack journeyMonitoringHttpManagerKey)) url (client (Proxy @JourneyMonitoringTokenizeAPI) authData) "generateToken" (Proxy @JourneyMonitoringTokenizeAPI) >>= checkJourneyMonitoringTokenizeResponse url

checkJourneyMonitoringTokenizeResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse ->
  m JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse
checkJourneyMonitoringTokenizeResponse url resp = fromEitherM (journeyMonitoringError url) resp >>= validateJourneyMonitoringTokenizeResponse

journeyMonitoringError :: BaseUrl -> ClientError -> ExternalAPICallError
journeyMonitoringError = ExternalAPICallError (Just "JOURNEY_MONITORING_API_ERROR")

validateJourneyMonitoringTokenizeResponse :: (MonadThrow m, Log m) => JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse -> m JourneyMonitoringTypes.JourneyMonitoringTokenizeResponse
validateJourneyMonitoringTokenizeResponse resp = do
  logDebug $ "JourneyMonitoring Tokenize Response: " <> show resp
  pure resp

prepareJourneyMonitoringHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareJourneyMonitoringHttpManager timeout =
  HMap.singleton (DT.pack journeyMonitoringHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

journeyMonitoringHttpManagerKey :: String
journeyMonitoringHttpManagerKey = "JourneyMonitoring-http-manager"

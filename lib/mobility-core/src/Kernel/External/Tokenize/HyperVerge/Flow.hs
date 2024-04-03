{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Tokenize.HyperVerge.Flow where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import EulerHS.Types (EulerClient, client)
import Kernel.External.Tokenize.HyperVerge.Error
import qualified Kernel.External.Tokenize.HyperVerge.Types as HyperVergeTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type HyperVergeTokenizeAPI =
  "login"
    :> ReqBody '[JSON] HyperVergeTypes.HyperVergeTokenizeRequest
    :> Post '[JSON] HyperVergeTypes.HyperVergeTokenizeResponse

tokenizeClient :: HyperVergeTypes.HyperVergeTokenizeRequest -> EulerClient HyperVergeTypes.HyperVergeTokenizeResponse
tokenizeClient = client (Proxy :: Proxy HyperVergeTokenizeAPI)

tokenize ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  HyperVergeTypes.HyperVergeTokenizeRequest ->
  m HyperVergeTypes.HyperVergeTokenizeResponse
tokenize url req = do
  callAPI url (tokenizeClient req) "HV-tokenize-API" (Proxy @HyperVergeTokenizeAPI) >>= checkHyperVergeTokenizeResponse url

checkHyperVergeTokenizeResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError HyperVergeTypes.HyperVergeTokenizeResponse ->
  m HyperVergeTypes.HyperVergeTokenizeResponse
checkHyperVergeTokenizeResponse url resp = fromEitherM (hyperVergeError url) resp >>= validateHyperVergeTokenizeResponse

hyperVergeError :: BaseUrl -> ClientError -> ExternalAPICallError
hyperVergeError = ExternalAPICallError (Just "HYPERVERGE_API_ERROR")

validateHyperVergeTokenizeResponse :: (MonadThrow m, Log m) => HyperVergeTypes.HyperVergeTokenizeResponse -> m HyperVergeTypes.HyperVergeTokenizeResponse
validateHyperVergeTokenizeResponse resp = do
  logDebug $ "HyperVerge Tokenize Response: " <> show resp
  case resp.statusCode of
    (Just "401") -> throwError $ HVUnauthorizedError
    (Just "400") -> throwError $ HVBadRequestError (fromMaybe "" resp.error)
    (Just "200") -> return resp
    _ -> throwError $ HVError ("The response from HV is : " <> show resp)

prepareHyperVergeHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareHyperVergeHttpManager timeout =
  HMap.singleton (DT.pack hyperVergeHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

hyperVergeHttpManagerKey :: String
hyperVergeHttpManagerKey = "hyperverge-http-manager"

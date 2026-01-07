{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.Verification.Tten.Flow where

import EulerHS.Types (EulerClient, client)
import qualified Kernel.External.Verification.Tten.Types as TtenTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type GetApplicationDetailsAPI =
  "api"
    :> "user"
    :> "getTTENApplicationDetailsByApplicantDetailsV1"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] TtenTypes.ApplicationDetailsReq
    :> Post '[JSON] TtenTypes.ApplicationDetailsResp

getApplicationDetailsClient :: Maybe Text -> TtenTypes.ApplicationDetailsReq -> EulerClient TtenTypes.ApplicationDetailsResp
getApplicationDetailsClient = client (Proxy :: Proxy GetApplicationDetailsAPI)

getApplicationDetails ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TtenTypes.TtenVerificationCfg ->
  Text ->
  TtenTypes.ApplicationDetailsReq ->
  m TtenTypes.ApplicationDetailsResp
getApplicationDetails TtenTypes.TtenVerificationCfg {..} token req =
  callAPI' Nothing url (getApplicationDetailsClient (Just $ "Bearer " <> token) req) "TTEN-GET_APPLICATION_DETAILS-API" (Proxy @GetApplicationDetailsAPI) >>= checkApplicationDetailsResponse url

checkApplicationDetailsResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError TtenTypes.ApplicationDetailsResp ->
  m TtenTypes.ApplicationDetailsResp
checkApplicationDetailsResponse url resp = fromEitherM (ttenError url) resp >>= validateApplicationDetailsResponse

validateApplicationDetailsResponse :: (MonadThrow m, Log m) => TtenTypes.ApplicationDetailsResp -> m TtenTypes.ApplicationDetailsResp
validateApplicationDetailsResponse resp = do
  logDebug $ "TTEN Application Details Response: " <> show resp
  pure resp

ttenError :: BaseUrl -> ClientError -> ExternalAPICallError
ttenError = ExternalAPICallError (Just "TTEN_API_ERROR")

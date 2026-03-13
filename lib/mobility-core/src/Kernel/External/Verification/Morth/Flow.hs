{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.Verification.Morth.Flow where

import EulerHS.Types (EulerClient, client)
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Morth.Types as MorthTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

-- ---------------------------------------------------------------------------
-- Servant API definition
-- POST /vehicle/getVehicleValidityInfo
-- ---------------------------------------------------------------------------

type GetVehicleValidityAPI =
  "vehicle"
    :> "getVehicleValidityInfo"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] MorthTypes.VehicleValidityReq
    :> Post '[JSON] MorthTypes.VehicleValidityResp

getVehicleValidityClient :: Maybe Text -> MorthTypes.VehicleValidityReq -> EulerClient MorthTypes.VehicleValidityResp
getVehicleValidityClient = client (Proxy :: Proxy GetVehicleValidityAPI)

-- ---------------------------------------------------------------------------
-- Servant API definition
-- POST /vehicle/getVehicleBasicInfo
-- ---------------------------------------------------------------------------

type GetVehicleBasicAPI =
  "vehicle"
    :> "getVehicleBasicInfo"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] MorthTypes.VehicleBasicInfoReq
    :> Post '[JSON] MorthTypes.VehicleBasicInfoResp

getVehicleBasicClient :: Maybe Text -> MorthTypes.VehicleBasicInfoReq -> EulerClient MorthTypes.VehicleBasicInfoResp
getVehicleBasicClient = client (Proxy :: Proxy GetVehicleBasicAPI)

-- ---------------------------------------------------------------------------
-- High-level call
-- ---------------------------------------------------------------------------

getVehicleValidityInfo ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  MorthTypes.MorthVerificationCfg ->
  MorthTypes.VehicleValidityReq ->
  m MorthTypes.VehicleValidityResp
getVehicleValidityInfo cfg req = do
  apiKey <- decrypt cfg.apiKey
  callAPI' Nothing cfg.url (getVehicleValidityClient (Just apiKey) req) "MORTH-GET_VEHICLE_VALIDITY_INFO" (Proxy @GetVehicleValidityAPI)
    >>= checkVehicleValidityResponse cfg.url

-- ---------------------------------------------------------------------------
-- High-level call: getVehicleBasicInfo
-- ---------------------------------------------------------------------------

getVehicleBasicInfo ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  MorthTypes.MorthVerificationCfg ->
  MorthTypes.VehicleBasicInfoReq ->
  m MorthTypes.VehicleBasicInfoResp
getVehicleBasicInfo cfg req = do
  apiKey <- decrypt cfg.apiKey
  callAPI' Nothing cfg.url (getVehicleBasicClient (Just apiKey) req) "MORTH-GET_VEHICLE_BASIC_INFO" (Proxy @GetVehicleBasicAPI)
    >>= checkVehicleBasicResponse cfg.url

-- ---------------------------------------------------------------------------
-- Response validation helpers
-- ---------------------------------------------------------------------------

checkVehicleValidityResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError MorthTypes.VehicleValidityResp ->
  m MorthTypes.VehicleValidityResp
checkVehicleValidityResponse url resp =
  fromEitherM (morthError url) resp >>= validateVehicleValidityResponse

validateVehicleValidityResponse :: (MonadThrow m, Log m) => MorthTypes.VehicleValidityResp -> m MorthTypes.VehicleValidityResp
validateVehicleValidityResponse resp = do
  logDebug $ "MoRTH Vehicle Validity Response: " <> show resp
  pure resp

checkVehicleBasicResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError MorthTypes.VehicleBasicInfoResp ->
  m MorthTypes.VehicleBasicInfoResp
checkVehicleBasicResponse url resp =
  fromEitherM (morthError url) resp >>= validateVehicleBasicResponse

validateVehicleBasicResponse :: (MonadThrow m, Log m) => MorthTypes.VehicleBasicInfoResp -> m MorthTypes.VehicleBasicInfoResp
validateVehicleBasicResponse resp = do
  logDebug $ "MoRTH Vehicle Basic Info Response: " <> show resp
  pure resp

morthError :: BaseUrl -> ClientError -> ExternalAPICallError
morthError = ExternalAPICallError (Just "MORTH_API_ERROR")

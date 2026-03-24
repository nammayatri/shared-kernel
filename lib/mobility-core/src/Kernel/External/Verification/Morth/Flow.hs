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

import qualified Data.Text as DT
import EulerHS.Types (EulerClient, ManagerSelector (..), client)
import Kernel.External.Encryption
import Kernel.External.Verification.Morth.Config (morthHttpManagerKey)
import qualified Kernel.External.Verification.Morth.Types as MorthTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..), GenericError (InternalError))
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
-- Servant API definition
-- POST /dl/getDrivinglicenseValidityInfo
-- ---------------------------------------------------------------------------

type GetDrivingLicenseValidityAPI =
  "dl"
    :> "getDrivinglicenseValidityInfo"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] MorthTypes.DrivingLicenseValidityReq
    :> Post '[JSON] MorthTypes.DrivingLicenseValidityResp

getDrivingLicenseValidityClient :: Maybe Text -> MorthTypes.DrivingLicenseValidityReq -> EulerClient MorthTypes.DrivingLicenseValidityResp
getDrivingLicenseValidityClient = client (Proxy :: Proxy GetDrivingLicenseValidityAPI)

-- ---------------------------------------------------------------------------
-- Servant API definition
-- POST /dl/getDrivinglicenseClassWiseValidity
-- ---------------------------------------------------------------------------

type GetDrivingLicenseClassWiseValidityAPI =
  "dl"
    :> "getDrivinglicenseClassWiseValidity"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] MorthTypes.DrivingLicenseClassWiseValidityReq
    :> Post '[JSON] MorthTypes.DrivingLicenseClassWiseValidityResp

getDrivingLicenseClassWiseValidityClient :: Maybe Text -> MorthTypes.DrivingLicenseClassWiseValidityReq -> EulerClient MorthTypes.DrivingLicenseClassWiseValidityResp
getDrivingLicenseClassWiseValidityClient = client (Proxy :: Proxy GetDrivingLicenseClassWiseValidityAPI)

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
  callAPI' (Just $ ManagerSelector $ DT.pack morthHttpManagerKey) cfg.url (getVehicleValidityClient (Just apiKey) req) "MORTH-GET_VEHICLE_VALIDITY_INFO" (Proxy @GetVehicleValidityAPI)
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
  callAPI' (Just $ ManagerSelector $ DT.pack morthHttpManagerKey) cfg.url (getVehicleBasicClient (Just apiKey) req) "MORTH-GET_VEHICLE_BASIC_INFO" (Proxy @GetVehicleBasicAPI)
    >>= checkVehicleBasicResponse cfg.url

-- ---------------------------------------------------------------------------
-- High-level call: getDrivingLicenseValidityInfo
-- ---------------------------------------------------------------------------

getDrivingLicenseValidityInfo ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  MorthTypes.MorthVerificationCfg ->
  MorthTypes.DrivingLicenseValidityReq ->
  m MorthTypes.DrivingLicenseValidityResp
getDrivingLicenseValidityInfo cfg req = do
  apiKey <- decrypt cfg.apiKey
  callAPI' Nothing cfg.url (getDrivingLicenseValidityClient (Just apiKey) req) "MORTH-GET_DRIVING_LICENSE_VALIDITY_INFO" (Proxy @GetDrivingLicenseValidityAPI)
    >>= checkDrivingLicenseValidityResponse cfg.url

-- ---------------------------------------------------------------------------
-- High-level call: getDrivinglicenseClassWiseValidity
-- ---------------------------------------------------------------------------

getDrivinglicenseClassWiseValidity ::
  ( HasCallStack,
    Log m,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  MorthTypes.MorthVerificationCfg ->
  MorthTypes.DrivingLicenseClassWiseValidityReq ->
  m MorthTypes.DrivingLicenseClassWiseValidityResp
getDrivinglicenseClassWiseValidity cfg req = do
  apiKey <- decrypt cfg.apiKey
  logDebug $ "MoRTH getDrivinglicenseClassWiseValidity request: " <> encodeToText req
  callAPI' Nothing cfg.url (getDrivingLicenseClassWiseValidityClient (Just apiKey) req) "MORTH-GET_DRIVING_LICENSE_CLASS_WISE_VALIDITY" (Proxy @GetDrivingLicenseClassWiseValidityAPI)
    >>= checkDrivingLicenseClassWiseValidityResponse cfg.url

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

checkDrivingLicenseValidityResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError MorthTypes.DrivingLicenseValidityResp ->
  m MorthTypes.DrivingLicenseValidityResp
checkDrivingLicenseValidityResponse url resp =
  fromEitherM (morthError url) resp >>= validateDrivingLicenseValidityResponse

validateDrivingLicenseValidityResponse :: (MonadThrow m, Log m) => MorthTypes.DrivingLicenseValidityResp -> m MorthTypes.DrivingLicenseValidityResp
validateDrivingLicenseValidityResponse resp = do
  logDebug $ "MoRTH Driving License Validity Response: " <> show resp
  when (resp.statusCode /= 200) $
    throwError $ InternalError $ "DL verification failed: statusCode " <> show resp.statusCode <> maybe "" (": " <>) resp.message
  pure resp

checkDrivingLicenseClassWiseValidityResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError MorthTypes.DrivingLicenseClassWiseValidityResp ->
  m MorthTypes.DrivingLicenseClassWiseValidityResp
checkDrivingLicenseClassWiseValidityResponse url resp =
  fromEitherM (morthError url) resp >>= validateDrivingLicenseClassWiseValidityResponse

validateDrivingLicenseClassWiseValidityResponse :: (MonadThrow m, Log m) => MorthTypes.DrivingLicenseClassWiseValidityResp -> m MorthTypes.DrivingLicenseClassWiseValidityResp
validateDrivingLicenseClassWiseValidityResponse resp = do
  logDebug $ "MoRTH Driving License Class Wise Validity Response: " <> show resp
  when (resp.statusCode /= 200) $
    throwError $ InternalError $ "DL class-wise verification failed: statusCode " <> show resp.statusCode <> maybe "" (": " <>) resp.message
  pure resp

morthError :: BaseUrl -> ClientError -> ExternalAPICallError
morthError = ExternalAPICallError (Just "MORTH_API_ERROR")

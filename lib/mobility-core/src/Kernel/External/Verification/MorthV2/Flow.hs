{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.Verification.MorthV2.Flow
  ( callToken,
    callVehicleByRegn,
    callVehicleByChasiEng,
    callLicenseDetails,
  )
where

import Data.Aeson (eitherDecodeStrict', encode, fromJSON)
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as DT
import EulerHS.Types (EulerClient, ManagerSelector (..), client)
import Kernel.External.Encryption (decrypt)
import Kernel.External.Verification.Morth.Config (morthHttpManagerKey)
import Kernel.External.Verification.MorthV2.Crypto
import qualified Kernel.External.Verification.MorthV2.Types as T
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..), MorthV2Error (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

-- ---------------------------------------------------------------------------
-- Servant API types
-- ---------------------------------------------------------------------------

type TokenAPI =
  "api"
    :> "auth"
    :> "token"
    :> ReqBody '[JSON] T.Envelope
    :> Post '[JSON] Value

type VehicleByRegnAPI =
  "api"
    :> "getVehicleDetailsByRegnNo"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] T.Envelope
    :> Post '[JSON] Value

type VehicleByChasiEngAPI =
  "api"
    :> "getVehicleDetailsByChasiNoAndEngNo"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] T.Envelope
    :> Post '[JSON] Value

type LicenseDetailsAPI =
  "api"
    :> "getLicenseDetails"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] T.Envelope
    :> Post '[JSON] Value

tokenClient :: T.Envelope -> EulerClient Value
tokenClient = client (Proxy :: Proxy TokenAPI)

vehicleByRegnClient :: Maybe Text -> T.Envelope -> EulerClient Value
vehicleByRegnClient = client (Proxy :: Proxy VehicleByRegnAPI)

vehicleByChasiEngClient :: Maybe Text -> T.Envelope -> EulerClient Value
vehicleByChasiEngClient = client (Proxy :: Proxy VehicleByChasiEngAPI)

licenseDetailsClient :: Maybe Text -> T.Envelope -> EulerClient Value
licenseDetailsClient = client (Proxy :: Proxy LicenseDetailsAPI)

-- ---------------------------------------------------------------------------
-- High-level calls
-- ---------------------------------------------------------------------------

type MorthV2M m r =
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  )

callToken ::
  MorthV2M m r =>
  T.MorthV2VerificationCfg ->
  m T.TokenRespPayload
callToken cfg = do
  apiKey <- decrypt cfg.apiKey
  envelope <- wrapRequest cfg.clientId apiKey (T.TokenReqPayload {clientId = cfg.clientId})
  resp <-
    callAPI' (Just managerSelector) cfg.url (tokenClient envelope) "MORTH_V2_TOKEN" (Proxy @TokenAPI)
      >>= fromEitherM (morthV2Error cfg.url)
  classify apiKey resp "MORTH_V2_TOKEN"

callVehicleByRegn ::
  MorthV2M m r =>
  T.MorthV2VerificationCfg ->
  Text ->
  T.VehicleByRegnReqPayload ->
  m T.VehicleDetailsResp
callVehicleByRegn cfg jwt req = do
  apiKey <- decrypt cfg.apiKey
  envelope <- wrapRequest cfg.clientId apiKey req
  resp <-
    callAPI' (Just managerSelector) cfg.url (vehicleByRegnClient (Just (bearer jwt)) envelope) "MORTH_V2_VEHICLE_REGN" (Proxy @VehicleByRegnAPI)
      >>= fromEitherM (morthV2Error cfg.url)
  classify apiKey resp "MORTH_V2_VEHICLE_REGN"

callVehicleByChasiEng ::
  MorthV2M m r =>
  T.MorthV2VerificationCfg ->
  Text ->
  T.VehicleByChasiEngReqPayload ->
  m T.VehicleDetailsResp
callVehicleByChasiEng cfg jwt req = do
  apiKey <- decrypt cfg.apiKey
  envelope <- wrapRequest cfg.clientId apiKey req
  resp <-
    callAPI' (Just managerSelector) cfg.url (vehicleByChasiEngClient (Just (bearer jwt)) envelope) "MORTH_V2_VEHICLE_CHASI_ENG" (Proxy @VehicleByChasiEngAPI)
      >>= fromEitherM (morthV2Error cfg.url)
  classify apiKey resp "MORTH_V2_VEHICLE_CHASI_ENG"

callLicenseDetails ::
  MorthV2M m r =>
  T.MorthV2VerificationCfg ->
  Text ->
  T.LicenseDetailsReqPayload ->
  m T.LicenseDetailsResp
callLicenseDetails cfg jwt req = do
  apiKey <- decrypt cfg.apiKey
  envelope <- wrapRequest cfg.clientId apiKey req
  resp <-
    callAPI' (Just managerSelector) cfg.url (licenseDetailsClient (Just (bearer jwt)) envelope) "MORTH_V2_LICENSE" (Proxy @LicenseDetailsAPI)
      >>= fromEitherM (morthV2Error cfg.url)
  classify apiKey resp "MORTH_V2_LICENSE"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

managerSelector :: ManagerSelector
managerSelector = ManagerSelector $ DT.pack morthHttpManagerKey

bearer :: Text -> Text
bearer t = "Bearer " <> t

-- | Build an outgoing encrypted envelope from an inner payload.
wrapRequest :: (MonadIO m, ToJSON a) => Text -> Text -> a -> m T.Envelope
wrapRequest clientId apiKey inner = do
  encData <- encryptEnvelopeData apiKey (LBS.toStrict (encode inner))
  pure T.Envelope {clientId, encData}

-- | Convert a raw response 'Value' into the typed body. The server returns
-- either an encrypted envelope, a plaintext object (observed for
-- @/auth/token@), or a plain @{status, message}@ error.
classify ::
  (FromJSON a, MonadThrow m, Log m) =>
  Text ->
  Value ->
  Text ->
  m a
classify apiKey body label =
  case fromJSON body :: Ae.Result T.Envelope of
    Ae.Success env -> case decryptEnvelopeData apiKey env.encData of
      Left err -> throwError (MorthV2UnexpectedResponse label ("decryption failed: " <> DT.pack (show err)))
      Right pt -> case eitherDecodeStrict' pt of
        Right a -> pure a
        Left _ -> case eitherDecodeStrict' pt :: Either String T.PlainError of
          Right pe -> throwError (plainErrorToMorthV2 pe)
          Left err -> throwError (MorthV2UnexpectedResponse label ("decrypted body did not parse: " <> DT.pack err))
    Ae.Error _ -> case fromJSON body :: Ae.Result T.PlainError of
      Ae.Success pe -> throwError (plainErrorToMorthV2 pe)
      Ae.Error _ -> case fromJSON body of
        Ae.Success a -> do
          logDebug $ "MorthV2 " <> label <> ": server returned plaintext (no envelope)"
          pure a
        Ae.Error err -> throwError (MorthV2UnexpectedResponse label (DT.pack err))

-- | Map the plain @{status, message}@ error catalogue (API docs section 4.1
-- + 4.2) to a typed 'MorthV2Error'. Falls back to 'MorthV2UnknownError' when
-- no pattern matches.
plainErrorToMorthV2 :: T.PlainError -> MorthV2Error
plainErrorToMorthV2 pe =
  let msg = pe.message
      lower = DT.toLower msg
   in if
          | "expired" `DT.isInfixOf` lower && "jwt" `DT.isInfixOf` lower -> MorthV2TokenExpired
          | "invalid" `DT.isInfixOf` lower && "jwt" `DT.isInfixOf` lower -> MorthV2TokenInvalid msg
          | "missing jwt" `DT.isInfixOf` lower || "jwt token is required" `DT.isInfixOf` lower -> MorthV2TokenMissing
          | "does not match clientid" `DT.isInfixOf` lower -> MorthV2ClientIdMismatch
          | "invalid clientid" `DT.isInfixOf` lower -> MorthV2InvalidClientId
          | "missing clientid" `DT.isInfixOf` lower -> MorthV2MissingClientId
          | "api key expired" `DT.isInfixOf` lower -> MorthV2ApiKeyExpired
          | "invalid api key" `DT.isInfixOf` lower -> MorthV2TokenInvalid msg
          | "decryption failed" `DT.isInfixOf` lower -> MorthV2DecryptionFailed msg
          | "data mapping failed" `DT.isInfixOf` lower -> MorthV2DataMappingFailed msg
          | "access denied" `DT.isInfixOf` lower && "not authorized" `DT.isInfixOf` lower -> MorthV2IpNotAuthorized msg
          | "audit certificate" `DT.isInfixOf` lower -> MorthV2AuditCertInvalid
          | "daily api limit" `DT.isInfixOf` lower -> MorthV2RateLimitExceeded
          | "user request not found" `DT.isInfixOf` lower -> MorthV2UserRequestNotFound msg
          | "internal server error" `DT.isInfixOf` lower -> MorthV2InternalServerError
          | "request processing failed" `DT.isInfixOf` lower -> MorthV2RequestProcessingFailed
          | otherwise -> MorthV2UnknownError (DT.pack (show pe.status)) msg

morthV2Error :: BaseUrl -> ClientError -> ExternalAPICallError
morthV2Error = ExternalAPICallError (Just "MORTH_V2_API_ERROR")

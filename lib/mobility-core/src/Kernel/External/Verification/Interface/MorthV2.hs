{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.MorthV2 where

import Control.Applicative ((<|>))
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Verification.Idfy.Types.Response as IdfyTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.MorthV2.Flow as Flow
import qualified Kernel.External.Verification.MorthV2.Token as Token
import Kernel.External.Verification.MorthV2.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common (MonadFlow, TryException (..))
import Kernel.Types.Error (MorthV2Error (..))
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (logInfo)
import Kernel.Utils.Servant.Client

withJwtRetry ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    Hedis.HedisFlow m r,
    TryException m
  ) =>
  MorthV2VerificationCfg ->
  Text ->
  (Text -> m a) ->
  m a
withJwtRetry cfg label call = do
  jwt <- Token.getCachedJwt cfg
  result <- withTryCatch ("morth_v2:" <> label) (call jwt)
  case result of
    Right r -> pure r
    Left err
      | isTokenError err -> do
        logInfo $ "MorthV2 " <> label <> ": token rejected (" <> T.pack (show err) <> "); refreshing and retrying once"
        Token.invalidateCachedJwt cfg.clientId
        jwt' <- Token.getCachedJwt cfg
        call jwt'
    Left err -> throwM err
  where
    isTokenError :: SomeException -> Bool
    isTokenError e = case fromException e of
      Just MorthV2TokenExpired -> True
      Just (MorthV2TokenInvalid _) -> True
      Just MorthV2TokenMissing -> True
      _ -> False

-- | Verify vehicle RC via MoRTH v2.1. Uses the regn-number endpoint when
-- @rcNumber@ is present, else falls back to chassis+engine.
verifyRCAsync ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    MonadFlow m,
    Hedis.HedisFlow m r,
    TryException m
  ) =>
  MorthV2VerificationCfg ->
  InterfaceTypes.VerifyRCReq ->
  m InterfaceTypes.VerifyRCResp
verifyRCAsync cfg req = do
  resp <-
    if not (T.null req.rcNumber)
      then withJwtRetry cfg "verifyRC" $ \jwt ->
        Flow.callVehicleByRegn cfg jwt (VehicleByRegnReqPayload {userId = cfg.clientId, regnNo = req.rcNumber})
      else case (req.engineNumber, req.chassisNumber) of
        (Just eng, Just chasi) ->
          -- Server wants trailing 5 chars of engine only.
          withJwtRetry cfg "verifyRC" $ \jwt ->
            Flow.callVehicleByChasiEng cfg jwt (VehicleByChasiEngReqPayload {userId = cfg.clientId, chasiNo = chasi, engNo = T.takeEnd 5 eng})
        (Nothing, Just _) -> throwError MorthV2EngineNumberRequired
        (Just _, Nothing) -> throwError MorthV2ChassisNumberRequired
        (Nothing, Nothing) -> throwError MorthV2VehicleIdentifierRequired
  pure $
    InterfaceTypes.SyncResp
      InterfaceTypes.VerifySyncResp
        { requestId = Nothing,
          requestor = VT.MorthV2,
          transactionId = Nothing,
          response = toRCVerificationResponse req resp
        }

-- | Verify driving license via MoRTH v2.1 (@/getLicenseDetails@).
verifyDL ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    MonadFlow m,
    Hedis.HedisFlow m r,
    TryException m
  ) =>
  MorthV2VerificationCfg ->
  InterfaceTypes.VerifyDLReq ->
  m InterfaceTypes.VerifyDLResp
verifyDL cfg req = do
  let dobStr = pack (formatTime defaultTimeLocale "%F" req.dateOfBirth)
      payload = LicenseDetailsReqPayload {dlNumber = req.dlNumber, dateOfBirth = dobStr, userId = cfg.clientId}
  resp <- withJwtRetry cfg "verifyDL" $ \jwt -> Flow.callLicenseDetails cfg jwt payload
  pure $
    InterfaceTypes.SyncDLResp
      InterfaceTypes.VerifyDLSyncResp
        { requestId = Nothing,
          requestor = VT.MorthV2,
          transactionId = Nothing,
          response = toDLVerificationResponse req resp
        }

-- ---------------------------------------------------------------------------
-- Response mapping
-- ---------------------------------------------------------------------------

-- | Map @vehicleData@ into the shared 'RCVerificationResponse'. Fields
-- gated by consent (owner, address, registration date, permit) stay
-- 'Nothing' until the account's @consentStatus@ is "ACTIVE".
toRCVerificationResponse :: InterfaceTypes.VerifyRCReq -> VehicleDetailsResp -> VT.RCVerificationResponse
toRCVerificationResponse req resp =
  let vfield k = lookupVehicleField k resp
      successful = fromMaybe False resp.success
   in VT.RCVerificationResponse
        { registrationNumber = resp.registrationNumber <|> Just req.rcNumber,
          registrationDate = vfield "rcRegnDt", -- consent-gated
          fitnessUpto = vfield "rcFitUpto",
          insuranceValidity = vfield "rcInsuranceUpto",
          vehicleClass = vfield "rcVhClassDesc",
          vehicleCategory = vfield "rcVhClassDesc",
          seatingCapacity = toJSON <$> (readMaybe . T.unpack =<< vfield "rcSeatCap" :: Maybe Int),
          manufacturer = vfield "rcMakerDesc",
          permitValidityFrom = vfield "rcPermitValidFrom", -- consent-gated
          permitValidityUpto = vfield "rcPermitValidUpto", -- consent-gated
          pucValidityUpto = vfield "rcPuccUpto",
          manufacturerModel = vfield "rcMakerModel",
          mYManufacturing = vfield "rcManuMonthYr",
          color = vfield "rcColor",
          fuelType = vfield "rcFuelDesc",
          bodyType = vfield "rcBodyTypeDesc",
          status = if successful then vfield "rcStatus" <|> Just "VALID" else Nothing,
          grossVehicleWeight = readMaybe . T.unpack =<< vfield "rcGvw",
          unladdenWeight = readMaybe . T.unpack =<< vfield "rcUnldWt"
        }

-- | Map @drivingLicenseData@ into 'DLVerificationOutputInterface'.
-- @dlCategory@ arrives comma-separated (e.g. @"NT, NT"@) and is split into
-- one 'CovDetail' per class.
toDLVerificationResponse :: InterfaceTypes.VerifyDLReq -> LicenseDetailsResp -> InterfaceTypes.DLVerificationOutputInterface
toDLVerificationResponse req resp =
  let dfield k = lookupLicenseField k resp
      validity = dfield "transportValidity" <|> dfield "nonTransportValidity"
      issue = dfield "dlIssueDate"
      categories =
        maybe [] (map T.strip . T.splitOn ",") (dfield "dlCategory")
      covs =
        categories <&> \cls ->
          IdfyTypes.CovDetail
            { category = Just cls,
              cov = cls,
              issue_date = issue
            }
      successful = fromMaybe False resp.success
   in InterfaceTypes.DLVerificationOutputInterface
        { driverName = Nothing, -- not returned without consent
          dob = Just (pack (formatTime defaultTimeLocale "%F" req.dateOfBirth)),
          licenseNumber = resp.drivingLicenseNumber <|> Just req.dlNumber,
          nt_validity_from = issue,
          nt_validity_to = validity,
          t_validity_from = issue,
          t_validity_to = validity,
          covs = Just covs,
          status = if successful then Just "VALID" else Nothing,
          dateOfIssue = issue,
          message = Nothing
        }

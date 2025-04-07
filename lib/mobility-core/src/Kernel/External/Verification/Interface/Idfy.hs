{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.Idfy
  ( module Reexport,
    verifyDLAsync,
    verifyRCAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractGSTImage,
    getTask,
    convertDLOutputToDLVerificationOutput,
    convertRCOutputToRCVerificationResponse,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Format
import Kernel.External.Encryption
import Kernel.External.Verification.Idfy.Auth as Reexport
import qualified Kernel.External.Verification.Idfy.Client as Idfy
import Kernel.External.Verification.Idfy.Config as Reexport
import Kernel.External.Verification.Idfy.Flow as Reexport
import Kernel.External.Verification.Idfy.Types as Reexport
import qualified Kernel.External.Verification.Idfy.Types.Request as Idfy
import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Error.Throwing

buildIdfyRequest :: MonadGuid m => Text -> a -> m (Idfy.IdfyRequest a)
buildIdfyRequest driverId a = do
  task_id <- generateGUID
  let group_id = driverId
  pure
    Idfy.IdfyRequest
      { _data = a,
        ..
      }

verifyDLAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let dobDay = T.pack $ formatTime defaultTimeLocale "%F" req.dateOfBirth
  let reqData =
        Idfy.DLVerificationData
          { id_number = req.dlNumber,
            date_of_birth = dobDay
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  idfySuccess <- Idfy.verifyDLAsync apiKey accountId url idfyReq
  pure $ VerifyAsyncResp {requestId = idfySuccess.request_id, requestor = VT.Idfy, transactionId = Nothing}

verifyRCAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  VerifyRCReq ->
  m VerifyRCResp
verifyRCAsync cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.RCVerificationData
          { rc_number = req.rcNumber,
            _a = Nothing
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  idfySuccess <- Idfy.verifyRCAsync apiKey accountId url idfyReq
  pure $ AsyncResp VerifyAsyncResp {requestId = idfySuccess.request_id, requestor = VT.Idfy, transactionId = Nothing}

validateImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage cfg req = do
  -- skipping validation for rc as validation not available in idfy
  case req.imageType of
    DriverLicense -> do
      let url = cfg.url
      apiKey <- decrypt cfg.apiKey
      accountId <- decrypt cfg.accountId
      let reqData =
            Idfy.ValidateRequest
              { document1 = req.image,
                doc_type = getDocType req.imageType
              }
      idfyReq <- buildIdfyRequest req.driverId reqData
      resp <- Idfy.validateImage apiKey accountId url idfyReq
      pure $ mkValidateImageResp resp
    VehicleRegistrationCertificate -> return validationNotAvailable
    VehiclePUC -> return validationNotAvailable
    VehiclePermit -> return validationNotAvailable
    VehicleInsurance -> return validationNotAvailable
    VehicleFitnessCertificate -> return validationNotAvailable
    VehicleNOC -> return validationNotAvailable
  where
    validationNotAvailable =
      ValidateImageResp
        { validationAvailable = False,
          detectedImage = Nothing
        }

mkValidateImageResp :: Idfy.IdfyResponse Idfy.ValidateResponse -> ValidateImageResp
mkValidateImageResp resp = do
  let detectedImage =
        resp.result <&> \result ->
          DetectedImage
            { imageType = getImageType result.detected_doc_type,
              isReadable = result.is_readable,
              confidence = result.readability.confidence
            }
  ValidateImageResp
    { validationAvailable = True,
      detectedImage
    }

getDocType :: ImageType -> Text
getDocType DriverLicense = "ind_driving_license"
getDocType VehicleRegistrationCertificate = "ind_rc"
getDocType VehiclePUC = "ind_puc" -- fix these
getDocType VehiclePermit = "ind_permit"
getDocType VehicleInsurance = "ind_insurance"
getDocType VehicleFitnessCertificate = "ind_fitness_certificate"
getDocType VehicleNOC = "ind_vehicle_noc"

getImageType :: Text -> ImageType
getImageType "ind_driving_license" = DriverLicense
getImageType "ind_rc" = VehicleRegistrationCertificate
getImageType "ind_puc" = VehiclePUC
getImageType "ind_permit" = VehiclePermit
getImageType "ind_insurance" = VehicleInsurance
getImageType "ind_fitness_certificate" = VehicleFitnessCertificate
getImageType "ind_vehicle_noc" = VehicleNOC
getImageType _ = VehicleRegistrationCertificate

extractRCImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.ExtractRequest
          { document1 = req.image1,
            document2 = req.image2
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  resp <- Idfy.extractRCImage apiKey accountId url idfyReq
  pure
    ExtractRCImageResp
      { extractedRC =
          resp.result <&> \result -> do
            ExtractedRC {rcNumber = result.extraction_output.registration_number}
      }

extractDLImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.ExtractRequest
          { document1 = req.image1,
            document2 = req.image2
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  resp <- Idfy.extractDLImage apiKey accountId url idfyReq
  pure
    ExtractDLImageResp
      { extractedDL =
          resp.result <&> \result -> do
            ExtractedDL
              { dlNumber = result.extraction_output.id_number,
                nameOnCard = result.extraction_output.name_on_card,
                dateOfBirth = result.extraction_output.date_of_birth
              }
      }

extractPanImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  ExtractPanImage ->
  m ExtractedPanImageResp
extractPanImage cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.ExtractRequest
          { document1 = req.image1,
            document2 = req.image2
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  resp <- Idfy.extractPanImage apiKey accountId url idfyReq
  pure
    ExtractedPanImageResp
      { extractedPan = resp.result >>= (\x -> pure x.extraction_output)
      }

extractGSTImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  ExtractGSTImage ->
  m ExtractedGSTImageResp
extractGSTImage cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.ExtractRequest
          { document1 = req.image1,
            document2 = req.image2
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  resp <- Idfy.extractGSTImage apiKey accountId url idfyReq
  pure
    ExtractedGSTImageResp
      { extractedGST = resp.result >>= (\x -> pure x.extraction_output)
      }

getTask ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  GetTaskReq ->
  (Text -> Maybe Text -> Text -> m ()) ->
  m GetTaskResp
getTask cfg req updateResp = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  (resp, respDump) <- Idfy.getTask apiKey accountId url req.requestId
  updateResp resp.status (Just respDump) req.requestId
  let dlOutput = join $ resp.result <&> (.source_output)
      rcOutput = join $ resp.result <&> (.extraction_output)
  case (dlOutput, rcOutput) of
    (Just op, Nothing) -> return $ DLResp (convertDLOutputToDLVerificationOutput op)
    (Nothing, Just op) -> return $ RCResp (convertRCOutputToRCVerificationResponse op)
    _ -> throwError $ InternalError ("Unrecognized response from getTesk api. Resp : " <> show resp)

convertDLOutputToDLVerificationOutput :: DLVerificationOutput -> DLVerificationOutputInterface
convertDLOutputToDLVerificationOutput DLVerificationOutput {..} =
  DLVerificationOutputInterface
    { driverName = name,
      licenseNumber = id_number,
      covs = cov_details,
      dateOfIssue = date_of_issue,
      message = Nothing,
      ..
    }

convertRCOutputToRCVerificationResponse :: RCVerificationOutput -> VT.RCVerificationResponse
convertRCOutputToRCVerificationResponse RCVerificationOutput {..} =
  VT.RCVerificationResponse
    { registrationDate = registration_date,
      registrationNumber = registration_number,
      fitnessUpto = fitness_upto,
      insuranceValidity = insurance_validity,
      vehicleClass = vehicle_class,
      vehicleCategory = vehicle_category,
      seatingCapacity = seating_capacity,
      manufacturer = manufacturer,
      permitValidityFrom = permit_validity_from,
      permitValidityUpto = permit_validity_upto,
      pucValidityUpto = puc_validity_upto,
      manufacturerModel = manufacturer_model,
      mYManufacturing = m_y_manufacturing,
      color = color <|> colour,
      fuelType = fuel_type,
      bodyType = body_type,
      status = status,
      grossVehicleWeight = gross_vehicle_weight >>= convertValueToFloat,
      unladdenWeight = unladden_weight >>= convertValueToFloat
    }

convertValueToFloat :: A.Value -> Maybe Float
convertValueToFloat (A.String val) = readMaybe (T.unpack val)
convertValueToFloat _ = Nothing

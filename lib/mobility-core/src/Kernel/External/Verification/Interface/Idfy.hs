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
    verifyPanAsync,
    verifyGstAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractGSTImage,
    extractAadhaarImage,
    getTask,
    convertDLOutputToDLVerificationOutput,
    convertRCOutputToRCVerificationResponse,
    nameCompare,
    convertPanOutputToPanVerification,
    convertGstOutputToGstVerification,
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
import Kernel.Utils.Servant.Client

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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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

verifyPanAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  VerifyPanAsyncReq ->
  m VerifyPanAsyncResp
verifyPanAsync cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let dobDay = T.pack $ formatTime defaultTimeLocale "%F" req.dateOfBirth
  let reqData =
        Idfy.PanVerificationData
          { id_number = req.panNumber,
            full_name = req.fullName,
            dob = dobDay
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  idfySuccess <- Idfy.verifyPanAsync apiKey accountId url idfyReq
  pure $ VerifyAsyncResp {requestId = idfySuccess.request_id, requestor = VT.Idfy, transactionId = Nothing}

verifyGstAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  VerifyGstAsyncReq ->
  m VerifyGstAsyncResp
verifyGstAsync cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.GstVerificationData
          { gstin = req.gstNumber,
            filing_details = req.filingDetails,
            e_invoice_details = req.eInvoiceDetails
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  idfySuccess <- Idfy.verifyGstAsync apiKey accountId url idfyReq
  pure $ VerifyAsyncResp {requestId = idfySuccess.request_id, requestor = VT.Idfy, transactionId = Nothing}

verifyRCAsync ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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

extractAadhaarImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IdfyCfg ->
  ExtractAadhaarImageReq ->
  m ExtractAadhaarImageRes
extractAadhaarImage cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.AadharVerificationData
          { document1 = req.image1,
            document2 = req.image2,
            consent = req.consent
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  resp <- Idfy.extractAadhaarImage apiKey accountId url idfyReq
  pure
    ExtractAadhaarImageRes
      { extractedAadhaar = resp.result
      }

nameCompare ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IdfyCfg ->
  NameCompareReq ->
  m NameCompareResp
nameCompare cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  let reqData =
        Idfy.NameCompareRequestBody
          { name1 = req.extractedName,
            name2 = req.verifiedName,
            percentage = req.percentage
          }
  idfyReq <- buildIdfyRequest req.driverId reqData
  resp <- Idfy.nameCompare apiKey accountId url idfyReq
  pure
    NameCompareResp
      { nameComparedData = resp.result
      }

getTask ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  IdfyCfg ->
  GetTaskReq ->
  (Text -> Maybe Text -> Text -> m ()) ->
  m GetTaskResp
getTask cfg req updateResp = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  (VerificationResponse resp, respDump) <- Idfy.getTask apiKey accountId url req.requestId
  updateResp resp.status (Just respDump) req.requestId
  result <- resp.result & fromMaybeM (InternalError ("Missing result in getTask response: " <> show resp))
  pure $ case result of
    DLResult (SourceOutput out) -> DLResp $ convertDLOutputToDLVerificationOutput out
    RCResult (ExtractionOutput out) -> RCResp $ convertRCOutputToRCVerificationResponse out
    PanResult (SourceOutput out) -> PanResp $ convertPanOutputToPanVerification out
    GstResult (SourceOutput out) -> GstResp $ convertGstOutputToGstVerification out

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

convertPanOutputToPanVerification :: PanVerificationOutput -> VT.PanVerificationResponse
convertPanOutputToPanVerification PanVerificationOutput {..} =
  VT.PanVerificationResponse
    { aadhaarSeedingStatus = aadhaar_seeding_status,
      panStatus = pan_status,
      nameMatch = name_match,
      dobMatch = dob_match,
      inputDetails = convertPanInputDetaills <$> input_details,
      status = status
    }

convertPanInputDetaills :: PanInputDetails -> VT.PanInputDetails
convertPanInputDetaills PanInputDetails {..} =
  VT.PanInputDetails
    { inputPanNumber = input_pan_number,
      inputName = input_name,
      inputDob = input_dob
    }

convertGstOutputToGstVerification :: GstVerificationOutput -> VT.GstVerificationResponse
convertGstOutputToGstVerification GstVerificationOutput {..} =
  VT.GstVerificationResponse
    { additionalPlaceOfBusinessFields = additional_place_of_business_fields,
      centreJurisdiction = centre_jurisdiction,
      centreJurisdictionCode = centre_jurisdiction_code,
      constitutionOfBusiness = constitution_of_business,
      dateOfCancellation = date_of_cancellation,
      dateOfRegistration = date_of_registration,
      gstin = gstin,
      gstinStatus = gstin_status,
      lastUpdatedDate = last_updated_date,
      legalName = legal_name,
      natureOfBusinessActivity = nature_of_business_activity,
      principalPlaceOfBusinessFields = principal_place_of_business_fields,
      source = source,
      stateJurisdictionCode = state_jurisdiction_code,
      status = status,
      taxpayerType = taxpayer_type,
      tradeName = trade_name,
      einvoiceStatus = einvoice_status,
      statusDetails = status_details,
      isSez = is_sez,
      filingDetails = filing_details
    }

convertValueToFloat :: A.Value -> Maybe Float
convertValueToFloat (A.String val) = readMaybe (T.unpack val)
convertValueToFloat _ = Nothing

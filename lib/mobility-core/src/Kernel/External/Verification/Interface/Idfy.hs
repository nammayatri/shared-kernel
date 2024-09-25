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
    getTask,
  )
where

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
  pure $ VerifyAsyncResp {requestId = idfySuccess.request_id, requestor = VT.Idfy}

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
  pure $ AsyncResp VerifyAsyncResp {requestId = idfySuccess.request_id, requestor = VT.Idfy}

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

getImageType :: Text -> ImageType
getImageType "ind_driving_license" = DriverLicense
getImageType "ind_rc" = VehicleRegistrationCertificate
getImageType "ind_puc" = VehiclePUC
getImageType "ind_permit" = VehiclePermit
getImageType "ind_insurance" = VehicleInsurance
getImageType "ind_fitness_certificate" = VehicleFitnessCertificate
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
                nameOnCard = result.extraction_output.name_on_card
              }
      }

getTask ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  IdfyCfg ->
  GetTaskReq ->
  m GetTaskResp
getTask cfg req = do
  let url = cfg.url
  apiKey <- decrypt cfg.apiKey
  accountId <- decrypt cfg.accountId
  IdfyStatus <$> Idfy.getTask apiKey accountId url req.requestId

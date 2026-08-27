{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.InternalScripts
  ( module Reexport,
    validateFace,
    validateImage,
    submitOCR,
    getOCRResultRC,
    getOCRResultDL,
    getOCRResultPAN,
    extractRCImageOCR,
    extractDLImageOCR,
    extractPANImageOCR,
  )
where

import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import Kernel.External.Verification.Interface.Types
import Kernel.External.Verification.InternalScripts.Error
import qualified Kernel.External.Verification.InternalScripts.FaceVerification as FV
import qualified Kernel.External.Verification.InternalScripts.InternalOCR as OCR
import Kernel.External.Verification.InternalScripts.Types as Reexport
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common hiding (ActorType (..))

validateImage :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalOCRCfg -> ValidateImageReq -> m ValidateImageResp
validateImage _cfg _req =
  pure
    ValidateImageResp
      { validationAvailable = False,
        detectedImage = Nothing
      }

validateFace :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => FaceVerificationCfg -> FaceValidationReq -> m FaceValidationRes
validateFace fvCfg req = do
  let url = fvCfg.url
  res <- FV.validateFace url req
  case res.faceType of
    UNKNOWN -> throwError PoorImageQuality
    FAKE_FACE -> throwError FakeFaceDetected
    REAL_FACE -> return res

submitOCR :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalOCRCfg -> OCRRequest -> m OCRAccepted
submitOCR = OCR.submitOCR

getOCRResultRC :: CacheFlow m r => Text -> m (Maybe ExtractedRC)
getOCRResultRC = OCR.getOCRResultRC

getOCRResultDL :: CacheFlow m r => Text -> m (Maybe ExtractedDL)
getOCRResultDL = OCR.getOCRResultDL

getOCRResultPAN :: CacheFlow m r => Text -> m (Maybe ExtractedPAN)
getOCRResultPAN = OCR.getOCRResultPAN

extractRCImageOCR :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalOCRCfg -> ExtractRCImageReq -> m ExtractRCImageResp
extractRCImageOCR cfg req = do
  _ <- OCR.submitOCR cfg $ OCRRequest {image = req.image1, imageType = VehicleRegistrationCertificate, driverId = req.driverId, prompt = Nothing}
  return $ ExtractRCImageResp {extractedRC = Just emptyExtractedRC, provider = Just VT.InternalOCR}

extractDLImageOCR :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalOCRCfg -> ExtractDLImageReq -> m ExtractDLImageResp
extractDLImageOCR cfg req = do
  _ <- OCR.submitOCR cfg $ OCRRequest {image = req.image1, imageType = DriverLicense, driverId = req.driverId, prompt = Nothing}
  return $ ExtractDLImageResp {extractedDL = Just emptyExtractedDL, provider = Just VT.InternalOCR}

emptyExtractedRC :: ExtractedRC
emptyExtractedRC =
  ExtractedRC
    { rcNumber = Nothing,
      vehicleClass = Nothing,
      manufacturer = Nothing,
      model = Nothing,
      fuelType = Nothing,
      colour = Nothing,
      chassisNumber = Nothing,
      engineNumber = Nothing,
      registrationDate = Nothing,
      ownerName = Nothing,
      manufacturingDate = Nothing,
      bodyType = Nothing
    }

extractPANImageOCR :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalOCRCfg -> ExtractPanImage -> m ExtractedPanImageResp
extractPANImageOCR cfg req = do
  _ <- OCR.submitOCR cfg $ OCRRequest {image = req.image1, imageType = PanCard, driverId = req.driverId, prompt = Nothing}
  return $ ExtractedPanImageResp {extractedPan = Just emptyExtractedPAN, provider = Just VT.InternalOCR}

emptyExtractedPAN :: Idfy.PanExtractionOutput
emptyExtractedPAN =
  Idfy.PanExtractionOutput
    { age = Nothing,
      date_of_birth = Nothing,
      date_of_issue = Nothing,
      fathers_name = Nothing,
      id_number = Nothing,
      is_scanned = Nothing,
      minor = Nothing,
      name_on_card = Nothing,
      pan_type = Nothing
    }

emptyExtractedDL :: ExtractedDL
emptyExtractedDL =
  ExtractedDL
    { dlNumber = Nothing,
      nameOnCard = Nothing,
      dateOfBirth = Nothing
    }

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module Kernel.External.Verification.Interface.Ekatra
  ( extractRCImage,
    extractDLImage,
    extractAadhaarImage,
    validateImage,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.External.Verification.Ekatra.Client (callEkatraOcrMap)
import Kernel.External.Verification.Ekatra.Types
import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import Kernel.External.Verification.Interface.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client (HasRequestId)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.IO (hClose, openBinaryTempFile)

type EkatraFlow m r =
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  )

validateImage ::
  EkatraFlow m r =>
  EkatraVerificationCfg ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage _cfg _req =
  pure
    ValidateImageResp
      { validationAvailable = False,
        detectedImage = Nothing
      }

extractRCImage ::
  EkatraFlow m r =>
  EkatraVerificationCfg ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage cfg req = do
  value <- runEkatraOcr cfg cfg.rcPrompt req.image1
  pure
    ExtractRCImageResp
      { extractedRC =
          Just
            ExtractedRC
              { rcNumber = findField ["rc_number", "registration_number"] value,
                vehicleClass = findField ["vehicle_class", "class"] value,
                manufacturer = findField ["manufacturer", "maker"] value,
                model = findField ["model"] value,
                fuelType = findField ["fuel_type", "fuel"] value,
                colour = findField ["colour", "color"] value,
                chassisNumber = findField ["chassis_number", "chassis"] value,
                engineNumber = findField ["engine_number"] value,
                registrationDate = findField ["registration_date"] value,
                ownerName = findField ["owner_name", "name"] value,
                manufacturingDate = findField ["manufacturing_date"] value,
                bodyType = findField ["body_type", "body"] value
              }
      }

extractDLImage ::
  EkatraFlow m r =>
  EkatraVerificationCfg ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage cfg req = do
  value <- runEkatraOcr cfg cfg.dlPrompt req.image1
  pure
    ExtractDLImageResp
      { extractedDL =
          Just
            ExtractedDL
              { dlNumber = findField ["dl_number", "license_number", "dlno"] value,
                nameOnCard = findField ["name", "name_on_card"] value,
                dateOfBirth = findField ["dob", "date_of_birth"] value
              }
      }

extractAadhaarImage ::
  EkatraFlow m r =>
  EkatraVerificationCfg ->
  ExtractAadhaarImageReq ->
  m ExtractAadhaarImageRes
extractAadhaarImage cfg req = do
  value <- runEkatraOcr cfg cfg.aadhaarPrompt req.image1
  let extractionOutput =
        Idfy.AadhaarExtractionOutput
          { address = findField ["address"] value,
            date_of_birth = findField ["dob", "date_of_birth"] value,
            district = findField ["district"] value,
            fathers_name = findField ["fathers_name", "father_name"] value,
            gender = findField ["gender", "sex"] value,
            house_number = findField ["house_number"] value,
            id_number = findField ["aadhaar_number", "uid", "id_number"] value,
            is_scanned = Nothing,
            name_on_card = findField ["name", "name_on_card"] value,
            pincode = findField ["pincode", "pin_code"] value,
            state = findField ["state"] value,
            street_address = findField ["street_address"] value,
            year_of_birth = findField ["year_of_birth"] value
          }
      qrOutput =
        Idfy.AadhaarQROutput
          { address = Nothing,
            date_of_birth = Nothing,
            district = Nothing,
            gender = Nothing,
            house_number = Nothing,
            id_number = Nothing,
            name_on_card = Nothing,
            pincode = Nothing,
            state = Nothing,
            street_address = Nothing,
            year_of_birth = Nothing
          }
  pure
    ExtractAadhaarImageRes
      { extractedAadhaar =
          Just
            Idfy.AadhaarResult
              { extraction_output = extractionOutput,
                qr_output = qrOutput
              }
      }

runEkatraOcr ::
  EkatraFlow m r =>
  EkatraVerificationCfg ->
  Text ->
  Text ->
  m A.Value
runEkatraOcr cfg prompt image1 = do
  apiKey <- decrypt cfg.apiKey
  let imageBytes = Base64.decodeLenient (TE.encodeUtf8 image1)
  tmpDir <- liftIO getTemporaryDirectory
  (filePath, tmpHandle) <- liftIO $ openBinaryTempFile tmpDir "ekatra_ocr_.jpg"
  let runOcr = do
        liftIO $ BS.hPut tmpHandle imageBytes
        liftIO $ hClose tmpHandle
        resp <-
          callEkatraOcrMap
            cfg.url
            apiKey
            prompt
            (fromMaybe False cfg.complexLayout)
            (fromMaybe False cfg.maskAadhaar)
            filePath
            (T.pack $ takeFileNameSafe filePath)
            "image/jpeg"
        pure $ getEkatraOcrValue resp
  runOcr `finallyM` liftIO (removePathForcibly filePath)
  where
    takeFileNameSafe = reverse . takeWhile (/= '/') . reverse
    finallyM action cleanup =
      (action <* cleanup) `catchAny` (\e -> cleanup >> throwM e)

findField :: [Text] -> A.Value -> Maybe Text
findField wanted = go
  where
    wantedSet = map normalizeKey wanted
    go (A.Object o) =
      let direct = listToMaybe [v | (k, v) <- KM.toList o, normalizeKey (AK.toText k) `elem` wantedSet]
       in case direct >>= valueToText of
            Just t | not (T.null t) -> Just t
            _ -> listToMaybe $ mapMaybe go (KM.elems o)
    go (A.Array xs) = listToMaybe $ mapMaybe go (toList xs)
    go (A.String s) = maybe Nothing go (A.decodeStrict (TE.encodeUtf8 s))
    go _ = Nothing

normalizeKey :: Text -> Text
normalizeKey = T.toLower . T.filter isAlphaNum

valueToText :: A.Value -> Maybe Text
valueToText (A.String s) = Just $ T.strip s
valueToText A.Null = Nothing
valueToText other = Just $ TE.decodeUtf8 $ BSL.toStrict $ A.encode other

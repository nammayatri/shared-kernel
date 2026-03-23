{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.Morth where

import Data.Text (pack)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Idfy.Types.Response as IdfyTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.Morth.Flow as MorthFlow
import Kernel.External.Verification.Morth.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (MorthError (..))
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Servant.Client

-- | Verify vehicle Registration Certificate (RC) validity via the MoRTH
-- Parivahan API.
--
-- The caller must populate the optional fields @engineNumber@ and
-- @chassisNumber@ in 'InterfaceTypes.VerifyRCReq'; if either or both are
-- absent a 'MorthError' (HTTP 400) is thrown with one of the error codes
-- @ENGINE_NUMBER_REQUIRED@, @CHASSIS_NUMBER_REQUIRED@, or
-- @ENGINE_NUMBER_AND_CHASSIS_NUMBER_REQUIRED@ so that the UI can surface the
-- error and prompt the user to supply the missing values.
verifyRCAsync ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  MorthVerificationCfg ->
  InterfaceTypes.VerifyRCReq ->
  m InterfaceTypes.VerifyRCResp
verifyRCAsync cfg req = do
  let missingEngNo = isNothing req.engineNumber
      missingChasiNo = isNothing req.chassisNumber
  when (missingEngNo && missingChasiNo) $
    throwError MorthEngineAndChassisNumberRequired
  engNo <- req.engineNumber & maybe (throwError MorthEngineNumberRequired) pure
  chasiNo <- req.chassisNumber & maybe (throwError MorthChassisNumberRequired) pure
  let morthReq = makeMorthReq req engNo chasiNo cfg.applicantMobile
  resp <- MorthFlow.getVehicleBasicInfo cfg morthReq
  let rcResp = convertToRCVerificationResponse req resp
  return $
    InterfaceTypes.SyncResp
      InterfaceTypes.VerifySyncResp
        { requestId = Nothing,
          requestor = VT.Morth,
          transactionId = Nothing,
          response = rcResp
        }
  where
    makeMorthReq :: InterfaceTypes.VerifyRCReq -> Text -> Text -> Text -> VehicleBasicInfoReq
    makeMorthReq InterfaceTypes.VerifyRCReq {rcNumber} engNo chasiNo applicantMobile =
      VehicleBasicInfoReq
        { regnNo = rcNumber,
          applicantMobile = applicantMobile,
          engNo = engNo,
          chasiNo = chasiNo
        }

    -- Convert the MoRTH getVehicleBasicInfo response into the common 'VT.RCVerificationResponse'.
    convertToRCVerificationResponse :: InterfaceTypes.VerifyRCReq -> VehicleBasicInfoResp -> VT.RCVerificationResponse
    convertToRCVerificationResponse InterfaceTypes.VerifyRCReq {..} VehicleBasicInfoResp {..} =
      VT.RCVerificationResponse
        { registrationDate = Nothing,
          registrationNumber = Just rcNumber,
          fitnessUpto = Nothing,
          insuranceValidity = Nothing,
          vehicleClass = data_ >>= (.vehcileCatg), -- vehcileClass is coming in code form the MoRTH API
          vehicleCategory = data_ >>= (.vehcileCatg),
          seatingCapacity = Nothing,
          manufacturer = data_ >>= (.maker),
          permitValidityFrom = Nothing,
          permitValidityUpto = Nothing,
          pucValidityUpto = Nothing,
          manufacturerModel = data_ >>= (.model),
          mYManufacturing = Nothing,
          color = data_ >>= (.color),
          fuelType = Nothing,
          bodyType = Nothing,
          status = if success then Just "VALID" else Nothing,
          grossVehicleWeight = Nothing,
          unladdenWeight = Nothing
        }

-- | Verify Driving License (DL) validity via the MoRTH Parivahan API.
-- Sync API: returns immediately with DL validity info.
-- The caller must supply @applicantMobile@ in 'InterfaceTypes.VerifyDLReq' for MoRTH.
verifyDL ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  MorthVerificationCfg ->
  InterfaceTypes.VerifyDLReq ->
  m InterfaceTypes.VerifyDLResp
verifyDL cfg req = do
  let dobStr = pack (formatTime defaultTimeLocale "%F" req.dateOfBirth)
      morthReq =
        DrivingLicenseClassWiseValidityReq
          { drivingLicense = req.dlNumber,
            applicantMobile = cfg.applicantMobile,
            dob = dobStr
          }
  resp <- MorthFlow.getDrivinglicenseClassWiseValidity cfg morthReq
  let dlResp = convertToDLVerificationResponse req resp
  return $
    InterfaceTypes.SyncDLResp
      InterfaceTypes.VerifyDLSyncResp
        { requestId = Nothing,
          requestor = VT.Morth,
          transactionId = Nothing,
          response = dlResp
        }
  where
    convertToDLVerificationResponse :: InterfaceTypes.VerifyDLReq -> DrivingLicenseClassWiseValidityResp -> InterfaceTypes.DLVerificationOutputInterface
    convertToDLVerificationResponse reqInner DrivingLicenseClassWiseValidityResp {..} =
      let dataList = fromMaybe [] data_
          validityDates =
            dataList
              <&> (.dlvalidityDate)
              & catMaybes
              & map T.strip
              & filter (\txt -> not (T.null txt) && T.toLower txt /= "not available")
          covDetails =
            dataList
              <&> \dlClassWiseData ->
                IdfyTypes.CovDetail
                  { category = dlClassWiseData.dlvehicleClass,
                    cov = fromMaybe "" dlClassWiseData.dlvehicleClass,
                    issue_date = dlClassWiseData.dlvalidityDate
                  }
          latestValidityDate = foldl' (\acc dt -> Just $ maybe dt (max dt) acc) Nothing validityDates
          dlStatus = Just $ if null validityDates then "INVALID" else "VALID"
       in InterfaceTypes.DLVerificationOutputInterface
            { driverName = Nothing,
              dob = Just $ pack (formatTime defaultTimeLocale "%F" reqInner.dateOfBirth),
              licenseNumber = Just reqInner.dlNumber,
              nt_validity_from = Nothing,
              nt_validity_to = latestValidityDate,
              t_validity_from = Nothing,
              t_validity_to = latestValidityDate,
              covs = Just covDetails,
              status = dlStatus,
              dateOfIssue = Nothing,
              message = message
            }

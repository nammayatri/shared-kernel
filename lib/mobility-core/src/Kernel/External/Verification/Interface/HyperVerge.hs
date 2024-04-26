{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Verification.Interface.HyperVerge where

import Kernel.External.Common.HyperVerge.HyperVergeErrors
import qualified Kernel.External.Verification.HyperVerge.Flow as HyperVergeFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import Kernel.External.Verification.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

validateRC ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeConfig ->
  InterfaceTypes.VerifyRCReq ->
  m InterfaceTypes.VerifyRCResp
validateRC cfg req = do
  let hvReq = makeHVRCValidationReq req
  hvResp <- HyperVergeFlow.validateRC cfg hvReq
  result <- fromMaybeM (HVRCValidationResultNotFound) hvResp.result
  rcData <- fromMaybeM (HVRCValidationInvalidRC hvReq.rc) result.resData.rcData
  return $ makeRCValidationResp rcData
  where
    makeHVRCValidationReq InterfaceTypes.VerifyRCReq {..} =
      HyperVergeTypes.HyperVergeRCValidationReq
        { rc = rcNumber,
          consent = "Y"
        }
    makeRCValidationResp HyperVergeTypes.RCData {..} =
      InterfaceTypes.SyncResp
        RCVerificationResponse
          { registrationDate = issueDate,
            registrationNumber = (Just req.rcNumber),
            fitnessUpto = expiryDate,
            insuranceValidity = insuranceData <&> (.expiryDate),
            vehicleClass = Nothing,
            vehicleCategory = vehicleData <&> (.category),
            seatingCapacity = toJSON <$> (vehicleData <&> (.seatingCapacity)),
            manufacturer = vehicleData <&> (.makerDescription),
            permitValidityFrom = permit_data <&> (.issue_date),
            permitValidityUpto = permit_data <&> (.expiry_date),
            pucValidityUpto = Nothing,
            manufacturerModel = vehicleData <&> (.makerModel),
            mYManufacturing = Nothing,
            colour = Nothing,
            color = vehicleData <&> (.color),
            fuelType = vehicleData <&> (.fuelType),
            bodyType = vehicleData <&> (.bodyType),
            status = Nothing
          }

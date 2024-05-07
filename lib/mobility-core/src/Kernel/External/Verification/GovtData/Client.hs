{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.GovtData.Client
  ( verifyRC,
  )
where

import qualified Kernel.External.Verification.GovtData.Storage.ClickHouse as QGC
import Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Error
import Kernel.Utils.Common

verifyRC ::
  (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  VerifyRCReq ->
  m VerifyRCResp
verifyRC req = do
  res <- QGC.findByRCNumber req.rcNumber >>= fromMaybeM (InternalError "rcNumber is not found in GovtData.")
  pure $
    SyncResp
      VT.RCVerificationResponse
        { registrationDate = Nothing,
          registrationNumber = res.registrationNumber,
          fitnessUpto = res.permitValidityUpto,
          insuranceValidity = res.permitValidityUpto,
          vehicleClass = res.bodyType,
          vehicleCategory = Nothing,
          seatingCapacity = toJSON <$> res.seatingCapacity,
          manufacturer = res.manufacturer,
          permitValidityFrom = res.permitValidityFrom,
          permitValidityUpto = res.permitValidityUpto,
          pucValidityUpto = Nothing,
          manufacturerModel = res.manufacturerModel,
          mYManufacturing = Nothing,
          colour = Nothing,
          color = Nothing,
          fuelType = res.fuelType,
          bodyType = res.bodyType,
          status = Nothing
        }

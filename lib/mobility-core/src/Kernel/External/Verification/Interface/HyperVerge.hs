{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.HyperVerge where

import Kernel.External.Encryption as Common (EncFlow)
import Kernel.External.SharedLogic.HyperVerge.Error (HyperVergeError (..))
import qualified Kernel.External.Verification.HyperVerge.Flow as HyperVergeFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Logging (Log)

verifySdkResp ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeVerificationCfg ->
  InterfaceTypes.VerifySdkDataReq ->
  m InterfaceTypes.VerifySdkDataResp
verifySdkResp config req = do
  let hvReq = convertVerifySdkRespToHyperVergeReq req
  resp <- HyperVergeFlow.verifySdkResp config hvReq
  return $ convertHvSdkVerificationRespToVerifySdkResp resp
  where
    convertVerifySdkRespToHyperVergeReq :: InterfaceTypes.VerifySdkDataReq -> HyperVergeTypes.HyperVergeSdkVerificationReq
    convertVerifySdkRespToHyperVergeReq InterfaceTypes.VerifySdkDataReq {..} = HyperVergeTypes.HyperVergeSdkVerificationReq {..}

    convertHvSdkVerificationRespToVerifySdkResp :: HyperVergeTypes.HyperVergeSdkVerificationRes -> InterfaceTypes.VerifySdkDataResp
    convertHvSdkVerificationRespToVerifySdkResp HyperVergeTypes.HyperVergeSdkVerificationRes {..} =
      InterfaceTypes.VerifySdkDataResp
        { userDetails = join $ result <&> (.userDetails),
          status = join $ result <&> (.status),
          transactionId = join $ result <&> (.transactionId)
        }

verifyRCAsync ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeVerificationCfg ->
  InterfaceTypes.VerifyRCReq ->
  m InterfaceTypes.VerifyRCResp
verifyRCAsync cfg req = do
  resp <- HyperVergeFlow.verifyRCAsync cfg $ makeHVRCAsyncReq req
  makeRCResp resp
  where
    makeHVRCAsyncReq :: InterfaceTypes.VerifyRCReq -> HyperVergeTypes.VerifyRCAsyncReq
    makeHVRCAsyncReq InterfaceTypes.VerifyRCReq {..} =
      HyperVergeTypes.VerifyRCAsyncReq
        { rc = rcNumber,
          consent = "Y"
        }
    makeRCResp :: (MonadThrow m, Log m) => HyperVergeTypes.VerifyRCAsyncResp -> m InterfaceTypes.VerifyRCResp
    makeRCResp rsp@HyperVergeTypes.VerifyRCAsyncResp {..} = InterfaceTypes.AsyncResp <$> (InterfaceTypes.VerifyAsyncResp <$> fromMaybeM (HVError $ "Could not find request id in a 200 response :" <> show rsp) (join (metaData <&> (.requestId))) <*> return VT.HyperVerge)

getVerificationStatus ::
  ( CoreMetrics m,
    EncFlow m r
  ) =>
  HyperVergeTypes.HyperVergeVerificationCfg ->
  InterfaceTypes.GetTaskReq ->
  m InterfaceTypes.GetTaskResp
getVerificationStatus cfg InterfaceTypes.GetTaskReq {..} = do
  resp <- fromMaybeM (HVBadInputError "workflowId compulsory for hyperverge get status api not specified !!!!") workflowId >>= flip (HyperVergeFlow.getVerificationStatus cfg) requestId
  convertToGetTaskResp resp
  where
    convertToGetTaskResp :: (MonadThrow m, Log m) => HyperVergeTypes.GetVerificationStatusResp -> m InterfaceTypes.GetTaskResp
    convertToGetTaskResp HyperVergeTypes.GetVerificationStatusResp {..} = do
      let apiResp = result <&> (.result) . (.apiOutput)
      apiData <- case workflowId of
        Just "fetchDetailedRC" -> fromMaybeM HVMissingPayloadError (join $ apiResp <&> (.rcData) . (.resultData))
        Just "checkDL" -> throwError $ InternalError "HyperVerge DL API not Integrated!!!!!"
        _ -> throwError $ InternalError "Unknown HypervVerge Workflow"
      return $
        InterfaceTypes.HyperVergeStatus
          VT.RCVerificationResponse
            { registrationDate = apiData.issueDate,
              registrationNumber = Nothing,
              fitnessUpto = Nothing,
              insuranceValidity = join $ apiData.insuranceData <&> (.expiryDate),
              vehicleClass = join $ apiData.vehicleData <&> (.category),
              vehicleCategory = Nothing,
              seatingCapacity = toJSON <$> (join $ apiData.vehicleData <&> (.seatingCapacity)),
              manufacturer = join $ apiData.vehicleData <&> (.makerDescription),
              permitValidityFrom = Nothing,
              permitValidityUpto = join $ apiData.permitData <&> (.expiryDate),
              pucValidityUpto = Nothing,
              manufacturerModel = join $ apiData.vehicleData <&> (.makerDescription),
              mYManufacturing = join $ apiData.vehicleData <&> (.manufacturedDate),
              colour = Nothing,
              color = join $ apiData.vehicleData <&> (.color),
              fuelType = join $ apiData.vehicleData <&> (.fuelType),
              bodyType = join $ apiData.vehicleData <&> (.bodyType),
              status = Just status
            }

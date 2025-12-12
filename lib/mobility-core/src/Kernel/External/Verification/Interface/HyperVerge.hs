{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.HyperVerge where

import qualified Data.Text as T
import Data.Time
import qualified Data.Tuple.Extra as TE
import Kernel.External.Encryption as Common (EncFlow)
import Kernel.External.SharedLogic.HyperVerge.Error (HyperVergeError (..))
import qualified Kernel.External.Verification.HyperVerge.Flow as HyperVergeFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.Idfy.Types.Response as IdfyTypes
import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (InternalError))
import qualified Kernel.Types.MonadGuid as UID
import Kernel.Utils.Common (logError)
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Logging (Log)
import Kernel.Utils.Servant.Client

verifySdkResp ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
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
    EncFlow m r,
    MonadIO m,
    HasRequestId r,
    MonadReader r m
  ) =>
  HyperVergeTypes.HyperVergeRCDLVerificationConfig ->
  InterfaceTypes.VerifyRCReq ->
  m InterfaceTypes.VerifyRCResp
verifyRCAsync cfg req = do
  transactionId <- liftIO UID.generateGUIDTextIO
  resp <- HyperVergeFlow.verifyRCAsync cfg transactionId $ makeHVRCAsyncReq req
  makeRCResp resp
  where
    makeHVRCAsyncReq :: InterfaceTypes.VerifyRCReq -> HyperVergeTypes.VerifyRCAsyncReq
    makeHVRCAsyncReq InterfaceTypes.VerifyRCReq {..} =
      HyperVergeTypes.VerifyRCAsyncReq
        { reg_no = rcNumber
        }
    makeRCResp :: (MonadThrow m, Log m) => HyperVergeTypes.VerifyRCAsyncResp -> m InterfaceTypes.VerifyRCResp
    makeRCResp rsp@HyperVergeTypes.HyperVergeVerificationAsyncResp {..} = InterfaceTypes.AsyncResp <$> (InterfaceTypes.VerifyAsyncResp <$> fromMaybeM (HVError $ "Could not find request id in a 200 response : " <> show rsp) (join (metaData <&> (.requestId))) <*> return VT.HyperVergeRCDL <*> return (join (metaData <&> (.transactionId))))

verifyDLAsync ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadIO m,
    HasRequestId r,
    MonadReader r m
  ) =>
  HyperVergeTypes.HyperVergeRCDLVerificationConfig ->
  InterfaceTypes.VerifyDLAsyncReq ->
  m InterfaceTypes.VerifyDLAsyncResp
verifyDLAsync cfg req = do
  transactionId <- liftIO UID.generateGUIDTextIO
  resp <- HyperVergeFlow.verifyDLAsync cfg transactionId $ makeHVDLAsyncReq req
  makeDLResp resp
  where
    makeHVDLAsyncReq :: InterfaceTypes.VerifyDLAsyncReq -> HyperVergeTypes.HyperVergeDLVerificationReq
    makeHVDLAsyncReq InterfaceTypes.VerifyDLAsyncReq {..} =
      HyperVergeTypes.HyperVergeDLVerificationReq
        { returnState = fromMaybe False returnState,
          dob = T.pack $ formatTime defaultTimeLocale "%d-%m-%Y" dateOfBirth,
          ..
        }
    makeDLResp :: (MonadThrow m, Log m) => HyperVergeTypes.HyperVergeDLVerificationResp -> m InterfaceTypes.VerifyDLAsyncResp
    makeDLResp rsp@HyperVergeTypes.HyperVergeVerificationAsyncResp {..} = InterfaceTypes.VerifyAsyncResp <$> fromMaybeM (HVError $ "Could not find request id in a 200 response :" <> show rsp) (join (metaData <&> (.requestId))) <*> return VT.HyperVergeRCDL <*> return (join (metaData <&> (.transactionId)))

getVerificationStatus ::
  ( CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  HyperVergeTypes.HyperVergeRCDLVerificationConfig ->
  InterfaceTypes.GetTaskReq ->
  (Text -> Maybe Text -> Text -> m ()) ->
  m InterfaceTypes.GetTaskResp
getVerificationStatus cfg rqst@InterfaceTypes.GetTaskReq {..} updateResp = do
  (resp, respDump) <- fromMaybeM (HVBadInputError "workflowId compulsory for hyperverge get status api not specified !!!!") workflowId >>= flip (HyperVergeFlow.getVerificationStatus cfg) requestId
  updateResp resp.status (Just respDump) requestId
  convertToGetTaskResp resp
  where
    convertToGetTaskResp :: (MonadThrow m, Log m) => HyperVergeTypes.GetVerificationStatusResp -> m InterfaceTypes.GetTaskResp
    convertToGetTaskResp statRsp@HyperVergeTypes.GetVerificationStatusResp {..} = do
      let apiResp' = join $ result <&> (.result) . (.apiOutput)
      case apiResp' of
        Just (HyperVergeTypes.RCVerificationResultData apiResp) -> do
          apiData <- fromMaybeM (HVMissingPayloadError $ show apiResp) apiResp.rcInfo
          return $
            InterfaceTypes.RCResp
              VT.RCVerificationResponse
                { registrationDate = apiData.reg_date,
                  registrationNumber = apiData.reg_no,
                  fitnessUpto = apiData.fit_upto,
                  insuranceValidity = join $ apiData.vehicle_insurance_details <&> (.insurance_upto),
                  vehicleClass = apiData.vehicle_catg,
                  vehicleCategory = Nothing,
                  seatingCapacity = toJSON <$> apiData.vehicle_seat_capacity,
                  manufacturer = apiData.vehicle_manufacturer_name,
                  permitValidityFrom = join $ apiData.permit_details <&> (.permit_valid_from),
                  permitValidityUpto = join $ apiData.permit_details <&> (.permit_valid_upto),
                  pucValidityUpto = join $ apiData.vehicle_pucc_details <&> (.pucc_upto),
                  manufacturerModel = apiData.model,
                  mYManufacturing = T.concat <$> sequence [(show <$> apiData.manufacturing_mon), Just "-", (show <$> apiData.manufacturing_yr)],
                  color = apiData.color,
                  fuelType = apiData.fuel_descr,
                  bodyType = apiData.body_type,
                  status = Just status,
                  grossVehicleWeight = apiData.vehicle_gross_weight,
                  unladdenWeight = apiData.unladen_weight
                }
        Just (HyperVergeTypes.DLVerificationResultData HyperVergeTypes.DLVerificationData {..}) -> do
          let ((transporterValidFrom, transporterValidTo), (nonTransportValidFrom, nonTransportValidTo)) = flip (maybe ((Nothing, Nothing), (Nothing, Nothing))) validity $ (\HyperVergeTypes.DLValidityInfo {..} -> TE.both (maybe (Nothing, Nothing) (\d -> (listToMaybe d, listToMaybe $ reverse d)) . (T.splitOn " " <$>)) (transport, nonTransport))
          return $
            InterfaceTypes.DLResp
              InterfaceTypes.DLVerificationOutputInterface
                { driverName = name,
                  licenseNumber = dl_number,
                  nt_validity_from = formatValidityTimeFormat nonTransportValidFrom,
                  nt_validity_to = formatValidityTimeFormat nonTransportValidTo,
                  t_validity_from = formatValidityTimeFormat transporterValidFrom,
                  t_validity_to = formatValidityTimeFormat transporterValidTo,
                  covs = map (\covDets -> IdfyTypes.CovDetail {category = Nothing, issue_date = covDets.issue_date, cov = covDets.cov}) <$> cov_details,
                  status = result <&> (.status) . (.apiOutput),
                  dateOfIssue = reverseDateFormat <$> issue_date,
                  ..
                }
        _ -> if (result <&> ((.statusCode) &&& (.status)) . (.apiOutput)) == Just (422, "failure") then makeInvalidRCDLResp else throwError $ HVError ("Failed to parse getTask response data of Hyperverge properly. Resp : " <> show statRsp)

    formatValidityTimeFormat :: Maybe Text -> Maybe Text
    formatValidityTimeFormat = (reverseDateFormat <$>) . (\val -> bool val Nothing (val == Just ""))

    reverseDateFormat :: Text -> Text
    reverseDateFormat = T.intercalate "-" . reverse . T.splitOn "-"

    makeInvalidRCDLResp :: (MonadThrow m, Log m) => m InterfaceTypes.GetTaskResp
    makeInvalidRCDLResp = do
      logError $ "Invalid Document details in the request. Could not find in provider's database. Req : " <> show rqst
      case workflowId of
        Just "checkDL" ->
          return $
            InterfaceTypes.DLResp
              InterfaceTypes.DLVerificationOutputInterface
                { driverName = Nothing,
                  dob = Nothing,
                  licenseNumber = Nothing,
                  nt_validity_from = Nothing,
                  nt_validity_to = Nothing,
                  t_validity_from = Nothing,
                  t_validity_to = Nothing,
                  covs = Nothing,
                  status = Just "id_not_found",
                  dateOfIssue = Nothing,
                  message = Nothing
                }
        Just "RCVerification" ->
          return $
            InterfaceTypes.RCResp
              VT.RCVerificationResponse
                { registrationDate = Nothing,
                  registrationNumber = Nothing,
                  fitnessUpto = Nothing,
                  insuranceValidity = Nothing,
                  vehicleClass = Nothing,
                  vehicleCategory = Nothing,
                  seatingCapacity = Nothing,
                  manufacturer = Nothing,
                  permitValidityFrom = Nothing,
                  permitValidityUpto = Nothing,
                  pucValidityUpto = Nothing,
                  manufacturerModel = Nothing,
                  mYManufacturing = Nothing,
                  color = Nothing,
                  fuelType = Nothing,
                  bodyType = Nothing,
                  status = Just "failure",
                  grossVehicleWeight = Nothing,
                  unladdenWeight = Nothing
                }
        _ -> throwError $ InternalError ("Unknown Workflow!!!!!!!. workflowId : " <> show workflowId)

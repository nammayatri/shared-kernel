module Kernel.External.Verification.Interface.Tten where

import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.Tten.Flow as TtenFlow
import Kernel.External.Verification.Tten.Types
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest))
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Servant.Client

verifyTten ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TtenVerificationCfg ->
  InterfaceTypes.VerifyRCReq ->
  m InterfaceTypes.VerifyRCResp
verifyTten cfg req = do
  token <- req.token & fromMaybeM (InternalError "Token not found for Tten verification")
  resp <- TtenFlow.getApplicationDetails cfg token $ makeTtenVerificationReq req
  let mbFirstTtenDetail = listToMaybe resp.data_
  if (isNothing mbFirstTtenDetail)
    then do
      throwError $ InvalidRequest "Invalid Tten certificate number"
    else do
      case mbFirstTtenDetail of
        Just ttenDetail | ttenDetail.udin_number == req.udinNo -> do
          return $ InterfaceTypes.SyncResp $ convertTtenResponseToRCVerificationResponse ttenDetail
        _ -> do
          throwError $ InvalidRequest "UDIN number mismatch"
  where
    makeTtenVerificationReq :: InterfaceTypes.VerifyRCReq -> ApplicationDetailsReq
    makeTtenVerificationReq InterfaceTypes.VerifyRCReq {..} =
      ApplicationDetailsReq
        { mobile_no = "",
          tten_no = rcNumber,
          udin_no = ""
        }
    convertTtenResponseToRCVerificationResponse :: TtenDetails -> VT.RCVerificationResponse
    convertTtenResponseToRCVerificationResponse res =
      VT.RCVerificationResponse
        { registrationDate = Nothing,
          registrationNumber = res.tten_number,
          fitnessUpto = res.validity,
          insuranceValidity = res.validity,
          vehicleClass = Nothing,
          vehicleCategory = Just "TOTO",
          seatingCapacity = Nothing,
          manufacturer = res.dealer_name,
          permitValidityFrom = res.issue_date,
          permitValidityUpto = res.validity,
          pucValidityUpto = Nothing,
          manufacturerModel = res.vehicle_model,
          mYManufacturing = Nothing,
          color = res.vehicle_colour,
          fuelType = Nothing,
          bodyType = Nothing,
          status = Nothing,
          grossVehicleWeight = Nothing,
          unladdenWeight = Nothing
        }

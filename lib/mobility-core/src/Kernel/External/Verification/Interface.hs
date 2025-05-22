{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface
  ( module Reexport,
    verifyDLAsync,
    verifyRC,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractAadhaarImage,
    extractGSTImage,
    validateFaceImage,
    searchAgent,
    verifySdkResp,
    getTask,
  )
where

import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Verification.GovtData.Client as GovtData
import Kernel.External.Verification.GovtData.Storage.Beam as BeamGRC
import Kernel.External.Verification.GovtData.Types as Reexport
import Kernel.External.Verification.HyperVerge.Types as Reexport
import Kernel.External.Verification.Idfy.Config as Reexport
import qualified Kernel.External.Verification.Interface.HyperVerge as HyperVerge
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Interface.InternalScripts as IS
import qualified Kernel.External.Verification.Interface.SafetyPortal as SafetyPortal
import Kernel.External.Verification.Interface.Types as Reexport
import Kernel.External.Verification.InternalScripts.Types
import Kernel.External.Verification.SafetyPortal.Types
import Kernel.External.Verification.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

verifyDLAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.verifyDLAsync cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL cfg -> HyperVerge.verifyDLAsync cfg req

verifyRC ::
  ( EncFlow m r,
    CoreMetrics m,
    HasSchemaName BeamGRC.GovtDataRCT,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  (VerificationService -> m VerificationServiceConfig) ->
  [VerificationService] ->
  VerifyRCReq ->
  m RCRespWithRemPriorityList
verifyRC getServiceConfig verificationProvidersPriorityList req = do
  when (null verificationProvidersPriorityList) $ throwError $ InternalError "No verification service provider configured or exhausted all service providers !!!!"
  verifyRCWithFallback verificationProvidersPriorityList
  where
    verifyRCWithFallback [] = throwError $ InternalError "Not able to verify the RC with all the configured providers !!!!!"
    verifyRCWithFallback (preferredProvider : restProviders) = do
      logDebug $ "Calling verifyRC for provider : " <> show preferredProvider
      result <- try @_ @SomeException $ getServiceConfig preferredProvider >>= flip verifyRC' req
      case result of
        Left _ -> verifyRCWithFallback restProviders
        Right res -> return $ RCRespWithRemPriorityList res restProviders

verifyRC' ::
  ( EncFlow m r,
    CoreMetrics m,
    HasSchemaName BeamGRC.GovtDataRCT,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  VerificationServiceConfig ->
  VerifyRCReq ->
  m VerifyRCResp
verifyRC' serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.verifyRCAsync cfg req
  GovtDataConfig -> GovtData.verifyRC req
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL cfg -> HyperVerge.verifyRCAsync cfg req

validateImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.validateImage cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

validateFaceImage ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  VerificationServiceConfig ->
  FaceValidationReq ->
  m FaceValidationRes
validateFaceImage serviceConfig req = case serviceConfig of
  IdfyConfig _ -> throwError $ InternalError "Not Implemented!"
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig cfg -> IS.validateFace cfg req
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

extractRCImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractRCImage cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

extractDLImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractDLImage cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

extractPanImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractPanImage ->
  m ExtractedPanImageResp
extractPanImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractPanImage cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

extractGSTImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractGSTImage ->
  m ExtractedGSTImageResp
extractGSTImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractGSTImage cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

extractAadhaarImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractAadhaarImageReq ->
  m ExtractAadhaarImageRes
extractAadhaarImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractAadhaarImage cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

searchAgent ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  DriverBackgroundVerificationServiceConfig ->
  Agent ->
  m SearchAgentResponse
searchAgent serviceConfig req = case serviceConfig of
  SafetyPortalConfig cfg -> SafetyPortal.searchAgent cfg req

verifySdkResp ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  VerifySdkDataReq ->
  m VerifySdkDataResp
verifySdkResp serviceConfig req = case serviceConfig of
  IdfyConfig _ -> throwError $ InternalError "Not Implemented!"
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig cfg -> HyperVerge.verifySdkResp cfg req
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"

getTask ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  GetTaskReq ->
  (Text -> Maybe Text -> Text -> m ()) ->
  m GetTaskResp
getTask serviceConfig req updateResp = case serviceConfig of
  IdfyConfig cfg -> Idfy.getTask cfg req updateResp
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL cfg -> HyperVerge.getVerificationStatus cfg req updateResp

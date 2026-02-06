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
    verifyPanAsync,
    verifyGstAsync,
    verifyRC,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractAadhaarImage,
    extractGSTImage,
    extractUdyogAadhaarAsync,
    validateFaceImage,
    searchAgent,
    verifySdkResp,
    getTask,
    nameCompare,
    fetchAndExtractVerifiedDL,
    getFile,
    pullDrivingLicense,
    fetchAndExtractVerifiedPan,
    fetchAndExtractVerifiedAadhaar,
    getVerifiedAadhaarXML,
  )
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import qualified Kernel.External.Verification.GovtData.Client as GovtData
import Kernel.External.Verification.GovtData.Storage.Beam as BeamGRC
import Kernel.External.Verification.GovtData.Types as Reexport
import Kernel.External.Verification.HyperVerge.Types as Reexport
import Kernel.External.Verification.Idfy.Config as Reexport
import qualified Kernel.External.Verification.Interface.Digilocker as DigiLocker
import qualified Kernel.External.Verification.Interface.HyperVerge as HyperVerge
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Interface.InternalScripts as IS
import qualified Kernel.External.Verification.Interface.SafetyPortal as SafetyPortal
import qualified Kernel.External.Verification.Interface.Tten as Tten
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
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

verifyPanAsync ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  VerificationServiceConfig ->
  VerifyPanAsyncReq ->
  m VerifyPanAsyncResp
verifyPanAsync serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.verifyPanAsync cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

verifyGstAsync ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  VerificationServiceConfig ->
  VerifyGstAsyncReq ->
  m VerifyGstAsyncResp
verifyGstAsync serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.verifyGstAsync cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

verifyRC ::
  ( EncFlow m r,
    CoreMetrics m,
    HasSchemaName BeamGRC.GovtDataRCT,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasRequestId r,
    MonadReader r m
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
      result <- withTryCatch "verifyRC" $ getServiceConfig preferredProvider >>= flip verifyRC' req
      case result of
        Left _ -> verifyRCWithFallback restProviders
        Right res -> return $ RCRespWithRemPriorityList res restProviders

verifyRC' ::
  ( EncFlow m r,
    CoreMetrics m,
    HasSchemaName BeamGRC.GovtDataRCT,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig cfg -> Tten.verifyTten cfg req

validateImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

validateFaceImage ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

extractRCImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

extractDLImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

extractPanImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

extractGSTImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

extractUdyogAadhaarAsync ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  VerificationServiceConfig ->
  ExtractUdyogAadhaarReq ->
  m VerifyAsyncResp
extractUdyogAadhaarAsync serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractUdyogAadhaarAsync cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

extractAadhaarImage ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

nameCompare ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  VerificationServiceConfig ->
  NameCompareReq ->
  m NameCompareResp
nameCompare serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.nameCompare cfg req
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfig _ -> throwError $ InternalError "Not Implemented!"
  HyperVergeVerificationConfigRCDL _ -> throwError $ InternalError "Not Implemented!"
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

searchAgent ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  DriverBackgroundVerificationServiceConfig ->
  Agent ->
  m SearchAgentResponse
searchAgent serviceConfig req = case serviceConfig of
  SafetyPortalConfig cfg -> SafetyPortal.searchAgent cfg req

verifySdkResp ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

getTask ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
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
  DigiLockerConfig _ -> throwError $ InternalError "Not Implemented!"
  TtenVerificationConfig _ -> throwError $ InternalError "Not Implemented!"

fetchAndExtractVerifiedDL ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasRequestId r
  ) =>
  VerificationServiceConfig ->
  DigiTypes.DigiLockerExtractDLReq ->
  m ExtractedDigiLockerDLResp
fetchAndExtractVerifiedDL serviceConfig req = case serviceConfig of
  DigiLockerConfig cfg -> DigiLocker.fetchAndExtractVerifiedDL cfg req.accessToken req.uri
  _ -> throwError $ InternalError "Not Implemented!"

getFile ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasRequestId r
  ) =>
  VerificationServiceConfig ->
  DigiTypes.DigiLockerGetFileReq ->
  m BSL.ByteString
getFile serviceConfig req = do
  logInfo $
    "Interface.getFile -> forwarding to DigiLocker | uri="
      <> req.uri
      <> ", accessTokenPresent="
      <> show (not $ T.null req.accessToken)
  case serviceConfig of
    DigiLockerConfig cfg -> DigiLocker.getFile cfg req.accessToken req.uri
    _ -> throwError $ InternalError "Not Implemented!"

pullDrivingLicense ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasRequestId r
  ) =>
  VerificationServiceConfig ->
  DigiTypes.DigiLockerPullDrivingLicenseReq ->
  m DigiTypes.DigiLockerPullDocumentResponse
pullDrivingLicense serviceConfig req = case serviceConfig of
  DigiLockerConfig cfg -> do
    let pullReq =
          DigiTypes.DigiLockerPullDrivingLicenseRequest
            { orgid = req.orgid,
              doctype = req.doctype,
              consent = req.consent,
              dlno = req.dlno
            }
    DigiLocker.pullDrivingLicense cfg req.accessToken pullReq
  _ -> throwError $ InternalError "Not Implemented!"

fetchAndExtractVerifiedPan ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasRequestId r
  ) =>
  VerificationServiceConfig ->
  DigiTypes.DigiLockerExtractPanReq ->
  m ExtractedDigiLockerPanResp
fetchAndExtractVerifiedPan serviceConfig req = case serviceConfig of
  DigiLockerConfig cfg -> DigiLocker.fetchAndExtractVerifiedPan cfg req.accessToken req.uri
  _ -> throwError $ InternalError "Not Implemented!"

fetchAndExtractVerifiedAadhaar ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m,
    MonadReader r m,
    HasField "requestId" r (Maybe Text)
  ) =>
  VerificationServiceConfig ->
  DigiTypes.DigiLockerExtractAadhaarReq ->
  m ExtractedDigiLockerAadhaarResp
fetchAndExtractVerifiedAadhaar serviceConfig req = do
  logInfo "Interface.fetchAndExtractVerifiedAadhaar -> delegating to DigiLocker"
  case serviceConfig of
    DigiLockerConfig cfg -> DigiLocker.fetchAndExtractVerifiedAadhaar cfg req.accessToken
    _ -> throwError $ InternalError "Not Implemented!"

getVerifiedAadhaarXML ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasRequestId r
  ) =>
  VerificationServiceConfig ->
  DigiTypes.DigiLockerExtractAadhaarReq ->
  m Text
getVerifiedAadhaarXML serviceConfig req = case serviceConfig of
  DigiLockerConfig cfg -> DigiLocker.getVerifiedAadhaarXML cfg req.accessToken
  _ -> throwError $ InternalError "Not Implemented!"

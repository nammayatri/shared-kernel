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
    validateFaceImage,
  )
where

import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Verification.GovtData.Client as GovtData
import Kernel.External.Verification.GovtData.Storage.Beam as BeamGRC
import Kernel.External.Verification.GovtData.Types as Reexport
import Kernel.External.Verification.Idfy.Config as Reexport
import qualified Kernel.External.Verification.Interface.HyperVerge as HyperVerge
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import qualified Kernel.External.Verification.Interface.InternalScripts as IS
import Kernel.External.Verification.Interface.Types as Reexport
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
  HyperVergeConfig _ -> throwError $ InternalError "Not Implemented!"

verifyRC ::
  ( EncFlow m r,
    CoreMetrics m,
    HasSchemaName BeamGRC.GovtDataRCT,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  [VerificationService] ->
  VerificationServiceConfig ->
  VerifyRCReq ->
  m VerifyRCResp
verifyRC verificationProvidersPriorityList idfyServiceConfig req = do
  when (null verificationProvidersPriorityList) $ throwError $ InternalError "No verification service provider configured"
  verifyRCWithFallback verificationProvidersPriorityList
  where
    verifyRCWithFallback [] = throwError $ InternalError "Not able to verify the RC with all the configured providers"
    verifyRCWithFallback (preferredProvider : restProviders) = do
      cfg <- case preferredProvider of
        Idfy -> pure idfyServiceConfig
        GovtData -> pure GovtDataConfig {}
        _ -> throwError $ InternalError "Not Implemented!"
      result <- try @_ @SomeException $ verifyRC' cfg req
      case result of
        Left _ -> verifyRCWithFallback restProviders
        Right res -> pure res

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
  HyperVergeConfig _ -> throwError $ InternalError "Not Implemented!"

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
  HyperVergeConfig _ -> throwError $ InternalError "Not Implemented!"

validateFaceImage ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  VerificationServiceConfig ->
  FaceValidationReq ->
  m FaceValidationResp
validateFaceImage serviceConfig req = case serviceConfig of
  IdfyConfig _ -> throwError $ InternalError "Not Implemented!"
  GovtDataConfig -> throwError $ InternalError "Not Implemented!"
  FaceVerificationConfig cfg -> IS.validateFace cfg req
  HyperVergeConfig cfg -> HyperVerge.validateFaceImage cfg req

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
  HyperVergeConfig _ -> throwError $ InternalError "Not Implemented!"

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
  HyperVergeConfig _ -> throwError $ InternalError "Not Implemented!"

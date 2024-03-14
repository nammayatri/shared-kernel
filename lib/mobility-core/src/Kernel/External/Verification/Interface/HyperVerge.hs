module Kernel.External.Verification.Interface.HyperVerge where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Verification.Error
import qualified Kernel.External.Verification.HyperVerge.Flow as HyperVergeFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVergeTypes
import qualified Kernel.External.Verification.InternalScripts.Types as InternalScriptsTypes
import qualified Kernel.External.Verification.Types as InterfaceTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Error.Throwing (throwError)

validateFaceImage :: (MonadFlow m, CoreMetrics m, Log m) => HyperVergeTypes.HyperVergeConfig -> InterfaceTypes.FaceValidationReq -> m InterfaceTypes.FaceValidationResp
validateFaceImage cfg req = do
  let url = cfg.hyperVergeFaceValidationUrl
  let appId = cfg.hyperVergeAppId
  let appKey = cfg.hyperVergeAppKey
  let hypeVergeReq = makeHyperVergeFaceValidationReq cfg req
  resp <- HyperVergeFlow.validateFaceImage url appId appKey hypeVergeReq
  convertToGenericResp cfg resp

makeHyperVergeFaceValidationReq :: HyperVergeTypes.HyperVergeConfig -> InterfaceTypes.FaceValidationReq -> HyperVergeTypes.FaceValidationReq
makeHyperVergeFaceValidationReq cfg InterfaceTypes.FaceValidationReq {..} =
  HyperVergeTypes.FaceValidationReq
    { selfie = (T.unpack . TE.decodeUtf8 . TE.encodeUtf8) filePath,
      uidType = cfg.hyperVergeUidType,
      ignoreSelfieQuality = cfg.hyperVergeIgnoreSelfieQuality,
      ..
    }

convertToGenericResp :: (MonadFlow m, CoreMetrics m, Log m) => HyperVergeTypes.HyperVergeConfig -> HyperVergeTypes.FaceValidationRespSuccess -> m InterfaceTypes.FaceValidationResp
convertToGenericResp cfg HyperVergeTypes.FaceValidationRespSuccess {..} = do
  faceType <- if result.resultData.match == "yes" && result.resultData.matchScore >= cfg.matchThreshold then (return InternalScriptsTypes.REAL_FACE) else throwError FakeFaceDetected
  return $
    InterfaceTypes.FaceValidationResp
      { score = fromIntegral result.resultData.matchScore,
        predictionCost = 0,
        ..
      }

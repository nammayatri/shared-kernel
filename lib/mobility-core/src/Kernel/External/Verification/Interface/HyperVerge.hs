module Kernel.External.Verification.Interface.HyperVerge where

import qualified Kernel.External.Verification.HyperVerge.Flow as HVFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVerge
import qualified Kernel.External.Verification.Interface.Types as Interface
import qualified Kernel.External.Verification.InternalScripts.Types as InternalScripts
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

validateFace :: (CoreMetrics m, MonadFlow m) => HyperVerge.HyperVergeConfig -> Interface.FaceValidationReq -> m Interface.FaceValidationRes
validateFace hvCfg req = do
  let url = hvCfg.url
  let appId = hvCfg.appId
  let appKey = hvCfg.appKey
  let transactionId = req.transactionId
  let hvReq = makeHyperVergeSelfieValidationReq req
  res <- HVFlow.callHyperVergeFaceValidationAPI url transactionId appId appKey hvReq
  makeHyperVergeSelfieValidationResp res
  where
    makeHyperVergeSelfieValidationReq Interface.FaceValidationReq {..} = HyperVerge.HyperVergeSelfieValidationReq {..}
    makeHyperVergeSelfieValidationResp HyperVerge.HyperVergeSelfieValidationSuccessRes {..} = do
      let faceType = case validationResult.details of
            Nothing -> InternalScripts.UNKNOWN
            Just HyperVerge.FaceDetails {..} -> case liveFace.value of
              "Yes" -> InternalScripts.REAL_FACE
              "No" -> InternalScripts.FAKE_FACE
              _ -> InternalScripts.UNKNOWN
          confidence = case validationResult.details of
            Nothing -> Nothing
            Just HyperVerge.FaceDetails {..} -> Just $ liveFace.confidence
      return $
        Interface.FaceValidationRes
          { score = Nothing,
            predictionCost = Nothing,
            ..
          }

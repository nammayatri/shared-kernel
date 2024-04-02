module Kernel.External.Verification.Interface.HyperVerge where

import qualified Kernel.External.Verification.HyperVerge.Flow as HVFlow
import qualified Kernel.External.Verification.HyperVerge.Types as HyperVerge
import qualified Kernel.External.Verification.Interface.Types as Interface
import qualified Kernel.External.Verification.InternalScripts.Types as InternalScripts
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Logging (logDebug)

validateFace :: (CoreMetrics m, MonadFlow m) => HyperVerge.HyperVergeConfig -> Interface.FaceValidationReq -> m Interface.FaceValidationRes
validateFace hvCfg req = do
  let url = hvCfg.url
  let appId = hvCfg.appId
  let appKey = hvCfg.appKey
  let transactionId = req.transactionId
  let hvReq = makeHyperVergeSelfieValidationReq req
  res <- HVFlow.callHyperVergeFaceValidationAPI url transactionId appId appKey hvReq
  logDebug $ "HyperVerge Response (before converting into Int. type) is :" <> show res --HVTODO: Remove this
  resp <- makeHyperVergeSelfieValidationResp res
  logDebug $ "HyperVerge Parsed Response is :" <> show resp --HVTODO: Remove this
  return resp
  where
    makeHyperVergeSelfieValidationReq Interface.FaceValidationReq {..} = HyperVerge.HyperVergeSelfieValidationReq {..}
    makeHyperVergeSelfieValidationResp HyperVerge.HyperVergeSelfieValidationRes {..} = do
      let (faceType, confidence) = case result of
            Nothing -> (InternalScripts.UNKNOWN, Nothing)
            Just r -> case r.details of
              Nothing -> (InternalScripts.UNKNOWN, Nothing)
              Just HyperVerge.FaceDetails {..} -> case liveFace.value of
                "yes" -> (InternalScripts.REAL_FACE, Just liveFace.confidence)
                "no" -> (InternalScripts.FAKE_FACE, Just liveFace.confidence)
                _ -> (InternalScripts.UNKNOWN, Just liveFace.confidence)
      return $
        Interface.FaceValidationRes
          { score = Nothing,
            predictionCost = Nothing,
            ..
          }

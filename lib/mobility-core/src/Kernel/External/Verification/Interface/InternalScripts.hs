{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.InternalScripts where

import Control.Monad
import qualified Kernel.External.Verification.Interface.Types as Interface
import Kernel.External.Verification.InternalScripts.Error
import qualified Kernel.External.Verification.InternalScripts.FaceVerification as FV
import qualified Kernel.External.Verification.InternalScripts.Types as InternalScripts
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

validateFace :: (CoreMetrics m, MonadFlow m) => InternalScripts.FaceVerificationCfg -> Interface.FaceValidationReq -> m Interface.FaceValidationRes
validateFace fvCfg req = do
  let url = fvCfg.url
  let intScrReq = makeInternalScriptsFaceValidationReq req
  res <- FV.validateFace url intScrReq
  case res.faceType of
    InternalScripts.UNKNOWN -> throwError PoorImageQuality
    InternalScripts.FAKE_FACE -> throwError FakeFaceDetected
    InternalScripts.REAL_FACE -> makeInterfaceFaceValidationResp res
  where
    makeInternalScriptsFaceValidationReq Interface.FaceValidationReq {..} = InternalScripts.FaceValidationReq {..}
    makeInterfaceFaceValidationResp InternalScripts.FaceValidationRes {..} =
      return $
        Interface.FaceValidationRes
          { confidence = Nothing,
            score = Just score,
            predictionCost = Just predictionCost,
            ..
          }

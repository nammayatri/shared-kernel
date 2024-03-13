{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Interface.InternalScripts
  ( validateFace,
  )
where

import Control.Monad
import Kernel.External.Verification.Error
import qualified Kernel.External.Verification.InternalScripts.FaceVerification as FV
import Kernel.External.Verification.InternalScripts.Types (FaceType (..), FaceVerificationCfg)
import qualified Kernel.External.Verification.InternalScripts.Types as InternalScriptsTypes
import qualified Kernel.External.Verification.Types as InterfaceTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

validateFace :: (CoreMetrics m, MonadFlow m) => FaceVerificationCfg -> InterfaceTypes.FaceValidationReq -> m InterfaceTypes.FaceValidationResp
validateFace fvCfg req = do
  let url = fvCfg.url
  res <- FV.validateFace url $ makeInternalScriptsReq req
  case res.faceType of
    UNKNOWN -> throwError PoorImageQuality
    FAKE_FACE -> throwError FakeFaceDetected
    REAL_FACE -> return $ makeInterfaceTypesResp res

makeInternalScriptsReq :: InterfaceTypes.FaceValidationReq -> InternalScriptsTypes.FaceValidationReq
makeInternalScriptsReq InterfaceTypes.FaceValidationReq {..} = InternalScriptsTypes.FaceValidationReq {..}

makeInterfaceTypesResp :: InternalScriptsTypes.FaceValidationRes -> InterfaceTypes.FaceValidationResp
makeInterfaceTypesResp InternalScriptsTypes.FaceValidationRes {..} = InterfaceTypes.FaceValidationResp {..}

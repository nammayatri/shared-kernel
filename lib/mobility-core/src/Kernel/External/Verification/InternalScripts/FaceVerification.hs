{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.InternalScripts.FaceVerification where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Kernel.External.Verification.Error
import Kernel.External.Verification.InternalScripts.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http
import Servant (JSON, Post, ReqBody, type (:>))

type FaceValidationAPI =
  "upload"
    :> ReqBody '[JSON] FaceValidationReq
    :> Post '[JSON] FaceValidationRes

validateFace :: (CoreMetrics m, MonadFlow m) => BaseUrl -> FaceValidationReq -> m FaceValidationRes
validateFace url req = callFaceVerificationApi url (translate req) "upload" faceValidationAPI

translate :: FaceValidationReq -> EulerClient FaceValidationRes
translate = client faceValidationAPI

faceValidationAPI :: Proxy FaceValidationAPI
faceValidationAPI = Proxy

prepareInternalScriptsHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareInternalScriptsHttpManager timeout =
  HMap.singleton (DT.pack internalScriptsManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

internalScriptsManagerKey :: String
internalScriptsManagerKey = "internal-scripts-http-manager"

callFaceVerificationApi :: CallAPI env api res
callFaceVerificationApi = callApiUnwrappingApiError (identity @FaceVerificationError) (Just $ ET.ManagerSelector $ DT.pack internalScriptsManagerKey) (Just "FACE_VERIFICATION_ERROR") Nothing

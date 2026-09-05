{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.InternalScripts.InternalImageDetection where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import EulerHS.Prelude
import EulerHS.Types (client)
import qualified EulerHS.Types as ET
import Kernel.External.Verification.Interface.Types
import Kernel.External.Verification.InternalScripts.Error
import Kernel.External.Verification.InternalScripts.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http
import Servant (JSON, Post, ReqBody, type (:>))

type DetectImageAPI =
  "detectImage"
    :> ReqBody '[JSON] OCRRequest
    :> Post '[JSON] FaceDetectionSummary

detectImageAPI :: Proxy DetectImageAPI
detectImageAPI = Proxy

detectImage :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalImageDetectionCfg -> OCRRequest -> m FaceDetectionSummary
detectImage cfg req = callImageDetectionApi cfg.url (client detectImageAPI req) "detectImage" detectImageAPI

callImageDetectionApi :: CallAPI m r api res
callImageDetectionApi = callApiUnwrappingApiError (identity @InternalImageDetectionError) (Just $ ET.ManagerSelector $ DT.pack internalImageDetectionManagerKey) (Just "INTERNAL_IMAGE_DETECTION_ERROR") Nothing

internalImageDetectionManagerKey :: String
internalImageDetectionManagerKey = "internal-image-detection-http-manager"

prepareInternalImageDetectionHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareInternalImageDetectionHttpManager timeout =
  HMap.singleton (DT.pack internalImageDetectionManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.InternalScripts.InternalOCR where

import qualified Data.Text as DT
import EulerHS.Prelude
import EulerHS.Types (client)
import qualified EulerHS.Types as ET
import Kernel.External.Verification.Interface.Types
import Kernel.External.Verification.InternalScripts.Error
import Kernel.External.Verification.InternalScripts.Types
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant (JSON, Post, ReqBody, type (:>))

type SubmitOCRAPI =
  "ocr"
    :> ReqBody '[JSON] OCRRequest
    :> Post '[JSON] OCRAccepted

submitOCRAPI :: Proxy SubmitOCRAPI
submitOCRAPI = Proxy

submitOCR :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => InternalOCRCfg -> OCRRequest -> m OCRAccepted
submitOCR cfg req = callOCRApi cfg.url (client submitOCRAPI req) "ocr" submitOCRAPI

getOCRResultRC :: CacheFlow m r => Text -> m (Maybe ExtractedRC)
getOCRResultRC driverId =
  Hedis.withCrossAppRedis $ Hedis.get ("providerPlatform:InternalOCR:RegistrationCertificate:" <> driverId)

getOCRResultDL :: CacheFlow m r => Text -> m (Maybe ExtractedDL)
getOCRResultDL driverId =
  Hedis.withCrossAppRedis $ Hedis.get ("providerPlatform:InternalOCR:DriverLicense:" <> driverId)

callOCRApi :: CallAPI m r api res
callOCRApi = callApiUnwrappingApiError (identity @InternalOCRError) (Just $ ET.ManagerSelector $ DT.pack internalOCRManagerKey) (Just "INTERNAL_OCR_ERROR") Nothing

internalOCRManagerKey :: String
internalOCRManagerKey = "internal-ocr-http-manager"

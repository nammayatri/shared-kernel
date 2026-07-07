{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Verification.Ekatra.Client
  ( callEkatraOcrMap,
  )
where

import qualified Data.Text as T
import EulerHS.Types as Euler
import qualified Kernel.External.Verification.Ekatra.Types as Ekatra
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

type EkatraOcrMapAPI =
  "v1"
    :> "ekatra"
    :> "ocr"
    :> "map"
    :> Header "Authorization" Text
    :> MultipartForm Tmp (MultipartData Tmp)
    :> Post '[JSON] Ekatra.EkatraOcrResponse

callEkatraOcrMap ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text -> -- api key
  Text -> -- prompt
  Bool -> -- complex_layout
  Bool -> -- mask_aadhaar
  FilePath -> -- local file path to the image
  Text -> -- display file name
  Text -> -- mime type
  m Ekatra.EkatraOcrResponse
callEkatraOcrMap url apiKey prompt complexLayout maskAadhaar filePath fileName mimeType = do
  boundary <- liftIO genBoundary
  let inputs =
        [ Input "prompt" prompt,
          Input "complex_layout" (boolToForm complexLayout),
          Input "mask_aadhaar" (boolToForm maskAadhaar)
        ]
      files =
        [ FileData
            "file"
            (if T.null fileName then "document" else fileName)
            (if T.null mimeType then "application/octet-stream" else mimeType)
            filePath
        ]
      multipartData = MultipartData inputs files
      eulerClient = Euler.client (Proxy @EkatraOcrMapAPI)
  callAPI' (Just $ Euler.ManagerSelector Ekatra.ekatraHttpManagerKey) url (eulerClient (Just $ "Bearer " <> apiKey) (boundary, multipartData)) "ekatraOcrMap" (Proxy @EkatraOcrMapAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Ekatra OCR map API: " <> show err)
  where
    boolToForm b = if b then "true" else "false"

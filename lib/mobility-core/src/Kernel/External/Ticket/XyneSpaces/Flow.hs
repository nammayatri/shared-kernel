{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Outbound HTTP clients for Xyne Spaces' @POST /api/apps/ticket/appDeskInbound@.
--
-- Two variants:
--
--   * 'appDeskInboundAPI' — @application/json@ body, used when the message has
--     no attachments.
--   * 'appDeskInboundMultipartAPI' — @multipart/form-data@, used when the
--     message has attachments. Caller is responsible for fetching attachment
--     URLs to local temp files before calling (see 'Interface.XyneSpaces').
module Kernel.External.Ticket.XyneSpaces.Flow
  ( appDeskInboundAPI,
    appDeskInboundMultipartAPI,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Types as Euler
import qualified Kernel.External.Ticket.XyneSpaces.Types as Xyne
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

type XyneAppDeskInboundAPI =
  "api"
    :> "apps"
    :> "ticket"
    :> "appDeskInbound"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Xyne.XyneInboundReq
    :> Post '[JSON] Xyne.XyneInboundResp

type XyneAppDeskInboundMultipartAPI =
  "api"
    :> "apps"
    :> "ticket"
    :> "appDeskInbound"
    :> Header "Authorization" Text
    :> MultipartForm Tmp (MultipartData Tmp)
    :> Post '[JSON] Xyne.XyneInboundResp

appDeskInboundAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Xyne.XyneInboundReq ->
  m Xyne.XyneInboundResp
appDeskInboundAPI url token req = do
  let eulerClient = Euler.client (Proxy @XyneAppDeskInboundAPI)
  callAPI url (eulerClient (Just $ "Bearer " <> token) req) "xyneAppDeskInboundAPI" (Proxy @XyneAppDeskInboundAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Xyne appDeskInbound API: " <> show err)

-- | Multipart variant. @tempFiles@ are local FS paths whose bytes will be
-- streamed as repeated @files@ form parts. Caller owns the lifecycle of those
-- paths (create + cleanup). Field name, file name, and mime type for each part
-- come from the @(formName, displayName, mimeType, filePath)@ tuple — Xyne
-- expects @formName@ to always be @"files"@.
appDeskInboundMultipartAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Xyne.XyneInboundReq ->
  [(Text, Text, Text, FilePath)] ->
  m Xyne.XyneInboundResp
appDeskInboundMultipartAPI url token req filePartsSpec = do
  boundary <- liftIO genBoundary
  let inputs =
        [ Input "channelId" req.channelId,
          Input "threadId" req.threadId,
          Input "subject" req.subject,
          Input "body" req.body
        ]
          <> maybe [] (\v -> [Input "externalId" v]) req.externalId
          <> maybe [] (\v -> [Input "senderName" v]) req.senderName
          <> maybe [] (\v -> [Input "senderEmail" v]) req.senderEmail
          -- Multipart carries the metadata dict as a single JSON-encoded
          -- form input (Xyne's server-side Zod schema accepts either an
          -- object on the JSON path or a JSON-string on the multipart path).
          <> maybe [] (\m -> if Map.null m then [] else [Input "additionalFormFields" (TE.decodeUtf8 . LBS.toStrict $ A.encode m)]) req.additionalFormFields
      files = map mkFileData filePartsSpec
      multipartData = MultipartData inputs files
      eulerClient = Euler.client (Proxy @XyneAppDeskInboundMultipartAPI)
  callAPI
    url
    (eulerClient (Just $ "Bearer " <> token) (boundary, multipartData))
    "xyneAppDeskInboundMultipartAPI"
    (Proxy @XyneAppDeskInboundMultipartAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Xyne appDeskInbound multipart API: " <> show err)
  where
    mkFileData (formName, displayName, mimeType, filePath) =
      FileData formName displayName (if T.null mimeType then "application/octet-stream" else mimeType) filePath

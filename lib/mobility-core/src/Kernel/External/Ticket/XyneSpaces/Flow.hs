{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Outbound HTTP clients for Xyne Spaces.
--
--   * 'appDeskInboundAPI' — @POST /api/apps/ticket/appDeskInbound@ with an
--     @application/json@ body; used when the message has no attachments.
--   * 'appDeskInboundMultipartAPI' — same endpoint as
--     @multipart/form-data@; used when the message has attachments. Caller
--     owns temp-file lifecycle (see 'Interface.XyneSpaces').
--   * 'updateTicketStatusAPI' — @POST /api/apps/ticket/updateTicket@,
--     status-only update against a Xyne ticket (@ticketId@ is Xyne's opaque
--     id, not our threadId).
module Kernel.External.Ticket.XyneSpaces.Flow
  ( appDeskInboundAPI,
    appDeskInboundMultipartAPI,
    updateTicketStatusAPI,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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
          -- Multipart flattens the metadata dict into repeated form inputs
          -- using bracket notation (@additionalFormFields[Category]=Dummy@).
          -- This is what Node multipart parsers (multer / formidable /
          -- busboy) reconstruct into a nested object — sending the map as a
          -- single JSON-encoded string leaves it as a raw string that the
          -- Xyne server-side @additionalFormFields@ handler ignores.
          <> maybe [] (map (\(k, v) -> Input ("additionalFormFields[" <> k <> "]") v) . Map.toList) req.additionalFormFields
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

type XyneUpdateTicketAPI =
  "api"
    :> "apps"
    :> "ticket"
    :> "updateTicket"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Xyne.XyneUpdateTicketReq
    :> Post '[JSON] A.Value

updateTicketStatusAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Xyne.XyneUpdateTicketReq ->
  m A.Value
updateTicketStatusAPI url token req = do
  let eulerClient = Euler.client (Proxy @XyneUpdateTicketAPI)
  callAPI url (eulerClient (Just $ "Bearer " <> token) req) "xyneUpdateTicketAPI" (Proxy @XyneUpdateTicketAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Xyne updateTicket API: " <> show err)

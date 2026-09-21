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
--   * 'updateCsatAPI' — @POST /api/csat/external/:ticketId@, submits a
--     customer satisfaction rating for a ticket. Authenticated via
--     @X-Api-Key@ rather than the Bearer token used by the other Xyne APIs.
--   * 'listTicketsAPI' — @POST /api/apps/ticket/list/search@, scoped,
--     filtered, cursor-paginated ticket listing.
--   * 'getTicketAPI' \/ 'getConversationAPI' — @GET /api/apps/ticket/{id}@
--     and @…/{id}/conversation@. Upstream failures throw the typed
--     'XyneError' (404 → 'XyneNotFound', 401\/403 → auth errors) via
--     'callApiUnwrappingApiError', following the MMI\/Twillio pattern.
--   * 'downloadFileAPI' — @GET /api/apps/files/download/{id}@ via raw
--     http-client, NOT 'callAPI': 'callAPI' JSON-logs response bodies, and
--     attachments can be megabytes of binary.
module Kernel.External.Ticket.XyneSpaces.Flow
  ( appDeskInboundAPI,
    appDeskInboundMultipartAPI,
    updateTicketStatusAPI,
    updateCsatAPI,
    listTicketsAPI,
    getTicketAPI,
    getConversationAPI,
    downloadFileAPI,
  )
where

import qualified Control.Exception as Exc
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import EulerHS.Types as Euler
import qualified Kernel.External.Ticket.XyneSpaces.Types as Xyne
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError), XyneError (..))
import Kernel.Utils.Common (fromEitherM, throwError)
import Kernel.Utils.Servant.Client
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import Network.HTTP.Types (hContentType)
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.URI (urlEncode)
import Servant hiding (throwError)
import qualified Servant.Client as SC

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

type XyneCsatAPI =
  "api"
    :> "csat"
    :> "external"
    :> Capture "ticketId" Text
    :> Header "X-Api-Key" Text
    :> ReqBody '[JSON] Xyne.XyneCsatReq
    :> Post '[JSON] A.Value

updateCsatAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Xyne.XyneCsatReq ->
  m A.Value
updateCsatAPI url apiKey ticketId req = do
  let eulerClient = Euler.client (Proxy @XyneCsatAPI)
  callAPI url (eulerClient ticketId (Just apiKey) req) "xyneUpdateCsatAPI" (Proxy @XyneCsatAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call Xyne CSAT API: " <> show err)

type XyneListTicketsAPI =
  "api"
    :> "apps"
    :> "ticket"
    :> "list"
    :> "search"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Xyne.XyneListTicketsReq
    :> Post '[JSON] Xyne.XyneListTicketsResp

listTicketsAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Xyne.XyneListTicketsReq ->
  m Xyne.XyneListTicketsResp
listTicketsAPI url token req = do
  -- Xyne's contract makes the scope mandatory; a scope-less request is a
  -- caller bug, so fail it here instead of round-tripping a 4xx.
  when (isNothing req.channelId && isNothing req.projectId && maybe True null req.boardIds) $
    throwError $ InternalError "Xyne list/search requires at least one scope: channelId, projectId or boardIds"
  let eulerClient = Euler.client (Proxy @XyneListTicketsAPI)
  callApiUnwrappingApiError (identity @XyneError) Nothing (Just "XYNE_LIST_TICKETS_ERROR") Nothing url (eulerClient (Just $ "Bearer " <> token) req) "xyneListTicketsAPI" (Proxy @XyneListTicketsAPI)

type XyneGetTicketAPI =
  "api"
    :> "apps"
    :> "ticket"
    :> Capture "ticketId" Text
    :> Header "Authorization" Text
    :> Get '[JSON] Xyne.XyneTicketDetail

getTicketAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m Xyne.XyneTicketDetail
getTicketAPI url token ticketId = do
  let eulerClient = Euler.client (Proxy @XyneGetTicketAPI)
  callApiUnwrappingApiError (identity @XyneError) Nothing (Just "XYNE_GET_TICKET_ERROR") Nothing url (eulerClient ticketId (Just $ "Bearer " <> token)) "xyneGetTicketAPI" (Proxy @XyneGetTicketAPI)

type XyneGetConversationAPI =
  "api"
    :> "apps"
    :> "ticket"
    :> Capture "ticketId" Text
    :> "conversation"
    :> Header "Authorization" Text
    :> Get '[JSON] Xyne.XyneConversationResp

getConversationAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m Xyne.XyneConversationResp
getConversationAPI url token ticketId = do
  let eulerClient = Euler.client (Proxy @XyneGetConversationAPI)
  callApiUnwrappingApiError (identity @XyneError) Nothing (Just "XYNE_GET_CONVERSATION_ERROR") Nothing url (eulerClient ticketId (Just $ "Bearer " <> token)) "xyneGetConversationAPI" (Proxy @XyneGetConversationAPI)

-- | Attachments can be megabytes of binary, and 'callAPI' JSON-encodes
-- response bodies into the log — so this one goes through http-client
-- directly. Returns the bytes and the upstream Content-Type; throws
-- 'XyneNotFound' on an unknown attachment id, matching the other read
-- clients.
downloadFileAPI ::
  MonadFlow m =>
  BaseUrl ->
  Text ->
  Text ->
  m (LBS.ByteString, Maybe Text)
downloadFileAPI url token attachmentId = do
  let encodedId = TE.decodeUtf8 $ urlEncode False (TE.encodeUtf8 attachmentId)
      fullUrl = SC.showBaseUrl url <> "/api/apps/files/download/" <> T.unpack encodedId
  respEither <- liftIO . Exc.try @HC.HttpException $ do
    manager <- HCT.getGlobalManager
    request <- HC.parseRequest fullUrl
    let request' =
          request
            { HC.requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))],
              HC.responseTimeout = HC.responseTimeoutMicro downloadTimeoutMicros,
              -- If Xyne ever 302s to object storage, the app JWT must not
              -- travel to the redirect target (presigned URLs carry their
              -- own auth in the query string).
              HC.shouldStripHeaderOnRedirect = (== "Authorization")
            }
    -- Streamed read with a hard cap so a huge or misbehaving upstream
    -- can't balloon memory. Reading cap+1 makes an oversize body
    -- detectable (> cap) instead of silently truncated at exactly cap.
    HC.withResponse request' manager $ \streamed -> do
      body <- HC.brReadSome (HC.responseBody streamed) (maxDownloadBytes + 1)
      pure (HC.responseStatus streamed, HC.responseHeaders streamed, body)
  (respStatus, respHeaders, body) <-
    fromEitherM (\err -> InternalError $ "Xyne file download could not reach " <> T.pack fullUrl <> ": " <> show err) respEither
  let status = HttpTypes.statusCode respStatus
      -- Lenient: a malformed byte in an upstream header must not throw
      -- past the HttpException boundary.
      mbContentType = TE.decodeUtf8With TEE.lenientDecode <$> List.lookup hContentType respHeaders
  if HttpTypes.statusIsSuccessful respStatus
    then
      if LBS.length body > fromIntegral maxDownloadBytes
        then throwError $ InternalError $ "Xyne attachment " <> attachmentId <> " exceeds the " <> show (maxDownloadBytes `div` (1024 * 1024)) <> " MB download cap"
        else pure (body, mbContentType)
    else case status of
      400 -> throwError XyneBadRequest
      401 -> throwError XyneUnauthorized
      403 -> throwError XyneAccessForbidden
      404 -> throwError XyneNotFound
      500 -> throwError XyneInternalServerError
      503 -> throwError XyneInternalServerError
      _ -> throwError XyneUnknownError
  where
    -- Attachments up to video size over slow links: 90s, mirroring the
    -- proven control-center client timeout.
    downloadTimeoutMicros = 90 * 1000000
    -- Generous for screenshots and screen recordings, small enough that
    -- concurrent downloads can't take the process down.
    maxDownloadBytes = 50 * 1024 * 1024

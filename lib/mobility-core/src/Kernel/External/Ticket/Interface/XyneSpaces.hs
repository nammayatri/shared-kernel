{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Xyne Spaces implementation of the generic ticket Interface.
--
-- Both 'createTicket' and 'updateTicket' map to the same endpoint
-- @POST /api/apps/ticket/appDeskInbound@. Xyne decides whether to open a new
-- ticket or append to an existing one based on whether the @threadId@ has been
-- seen before; we always use our @IssueReport.id@ as the threadId so reuse
-- across create/update is automatic.
--
-- Transport selection:
--
--   * If the request has no media files, we send @application/json@.
--   * If it has media files, we download each URL to a temp file and POST as
--     @multipart/form-data@ with a repeated @files@ part. Temp files are
--     cleaned up after the call (best-effort — they live under @/tmp@).
--
-- Sender attribution: 'createTicket' uses @req.name@ / @req.phoneNo@; on
-- 'updateTicket' there's no top-level name/phone field on 'UpdateTicketReq',
-- so we derive both from @rideDescription@ (preferring customer over driver)
-- before falling back to @Nothing@. Email is left @Nothing@ for now —
-- 'IssueManagement.Common.Person' has no email field.
--
-- Note on the returned @ticketId@: we echo back the threadId (= IssueReport id)
-- rather than Xyne's opaque ticketId so that the existing IssueManagement
-- caller can pass it back unchanged on subsequent updateTicket calls. Xyne's
-- own ticketId / xyneId / conversationId are logged but not propagated.
module Kernel.External.Ticket.Interface.XyneSpaces
  ( createTicket,
    updateTicket,
  )
where

import qualified Control.Exception as Exc
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as IT
import Kernel.External.Ticket.XyneSpaces.Config
import qualified Kernel.External.Ticket.XyneSpaces.Flow as XF
import qualified Kernel.External.Ticket.XyneSpaces.Types as Xyne
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (MonadFlow)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (logInfo, throwError)
import Kernel.Utils.Servant.Client
import Kernel.Utils.Time (showTimeIst)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)

createTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  XyneSpacesCfg ->
  IT.CreateTicketReq ->
  m IT.CreateTicketResp
createTicket config req = do
  threadId <- case req.issueId of
    Just t -> pure t
    Nothing -> throwError (InternalError "Xyne createTicket requires issueId (used as threadId)")
  token <- decrypt config.token
  let xyneReq =
        Xyne.XyneInboundReq
          { channelId = config.channelId,
            threadId = threadId,
            subject = buildSubject req.category req.rideDescription,
            body = buildBody req,
            externalId = Nothing,
            senderName = req.name,
            senderEmail = Nothing
          }
  resp <- callAppDeskInbound config token xyneReq (fromMaybe [] req.mediaFiles)
  logInfo $
    "Xyne createTicket: threadId=" <> threadId
      <> " xyneTicketId="
      <> resp.ticketId
      <> " xyneId="
      <> resp.xyneId
      <> " isNew="
      <> show resp.isNew
  pure
    IT.CreateTicketResp
      { ticketId = threadId,
        status = IT.Open,
        requesterId = Nothing
      }

updateTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  XyneSpacesCfg ->
  IT.UpdateTicketReq ->
  m IT.UpdateTicketResp
updateTicket config req = do
  -- req.ticketId is the value createTicket returned (= our threadId).
  let threadId = req.ticketId
  token <- decrypt config.token
  let (mbSenderName, mbSenderPhone) = senderInfoFromRide req.rideDescription
  let xyneReq =
        Xyne.XyneInboundReq
          { channelId = config.channelId,
            threadId = threadId,
            subject = buildUpdateSubject req,
            body = buildUpdateBody req mbSenderName mbSenderPhone,
            externalId = Nothing,
            senderName = mbSenderName,
            senderEmail = Nothing
          }
  let attachments =
        fromMaybe [] (req.issueDetails >>= (.mediaFiles))
  resp <- callAppDeskInbound config token xyneReq attachments
  logInfo $
    "Xyne updateTicket: threadId=" <> threadId
      <> " xyneTicketId="
      <> resp.ticketId
      <> " isNew="
      <> show resp.isNew
  -- Mirror Zendesk: return req.status (caller intent), not the parsed response.
  pure
    IT.UpdateTicketResp
      { ticketId = threadId,
        status = req.status,
        message = "Ticket updated"
      }

-- | Pick JSON vs multipart based on attachment presence. Downloads each
-- attachment URL to a temp file under @/tmp@ and cleans up afterwards (best
-- effort — leaked files are reaped by the OS).
callAppDeskInbound ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  XyneSpacesCfg ->
  Text ->
  Xyne.XyneInboundReq ->
  [Text] ->
  m Xyne.XyneInboundResp
callAppDeskInbound config token xyneReq attachmentUrls
  | null attachmentUrls = XF.appDeskInboundAPI config.url token xyneReq
  | otherwise = do
    tempSpecs <- L.runIO $ traverse fetchToTempFile attachmentUrls
    XF.appDeskInboundMultipartAPI config.url token xyneReq tempSpecs `finallyM` cleanupTemps tempSpecs
  where
    cleanupTemps specs =
      L.runIO . forM_ specs $ \(_, _, _, fp) -> do
        eRes <- Exc.try (removeFile fp)
        case eRes of
          Right () -> pure ()
          Left (_ :: SomeException) -> pure ()

-- | Run @cleanup@ regardless of whether @action@ succeeded.
finallyM :: MonadCatch m => m a -> m () -> m a
finallyM action cleanup =
  (action <* cleanup)
    `catchAny` (\e -> cleanup >> throwM e)

-- | Download @url@ into a fresh temp file under @/tmp@ and return
-- @(formName, displayName, mimeType, path)@ suitable for 'appDeskInboundMultipartAPI'.
-- Uses a fresh TLS manager per call — fine for low-volume webhook inbound.
fetchToTempFile :: Text -> IO (Text, Text, Text, FilePath)
fetchToTempFile url = do
  mgr <- HCT.newTlsManager
  req <- HC.parseRequest (T.unpack url)
  resp <- HC.httpLbs req mgr
  let body = HC.responseBody resp
      displayName = guessFilename url
      mimeType = guessMimeType displayName
  (path, hdl) <- openBinaryTempFile "/tmp" "xyne-attach-"
  LBS.hPut hdl body
  hClose hdl
  pure ("files", displayName, mimeType, path)

guessFilename :: Text -> Text
guessFilename url =
  let withoutQuery = T.takeWhile (/= '?') url
      parts = filter (not . T.null) (T.splitOn "/" withoutQuery)
   in case reverse parts of
        (last_ : _) -> last_
        [] -> "attachment"

guessMimeType :: Text -> Text
guessMimeType filename =
  case T.toLower (T.takeWhileEnd (/= '.') filename) of
    "png" -> "image/png"
    "jpg" -> "image/jpeg"
    "jpeg" -> "image/jpeg"
    "gif" -> "image/gif"
    "webp" -> "image/webp"
    "pdf" -> "application/pdf"
    "mp3" -> "audio/mpeg"
    "m4a" -> "audio/mp4"
    "wav" -> "audio/wav"
    "ogg" -> "audio/ogg"
    "webm" -> "video/webm"
    "mp4" -> "video/mp4"
    "txt" -> "text/plain"
    "html" -> "text/html"
    "json" -> "application/json"
    _ -> "application/octet-stream"

buildSubject :: Text -> Maybe IT.RideInfo -> Text
buildSubject category mbRide =
  category <> maybe "" (\r -> " - " <> r.rideShortId) mbRide

buildUpdateSubject :: IT.UpdateTicketReq -> Text
buildUpdateSubject req =
  let prefix = case req.issueDetails >>= (.category) of
        Just c -> c
        Nothing -> "Issue update"
   in prefix <> maybe "" (\r -> " - " <> r.rideShortId) req.rideDescription

buildBody :: IT.CreateTicketReq -> Text
buildBody IT.CreateTicketReq {..} =
  T.unlines $
    formatCategory category subCategory
      <> formatDescription issueDescription
      <> formatCustomer name phoneNo
      <> maybe [] formatRide rideDescription
      <> formatMedia mediaFiles

buildUpdateBody :: IT.UpdateTicketReq -> Maybe Text -> Maybe Text -> Text
buildUpdateBody IT.UpdateTicketReq {..} mbName mbPhone =
  T.unlines $
    [comment]
      <> formatCustomer mbName mbPhone
      <> maybe [] formatRide rideDescription
      <> maybe [] formatUpdateIssueDetails issueDetails

formatCategory :: Text -> Maybe Text -> [Text]
formatCategory category mbSubCategory =
  [ "=== Issue Category ===",
    "Category: " <> category
  ]
    <> maybe [] (\sc -> ["Sub Category: " <> sc]) mbSubCategory
    <> [""]

formatDescription :: Text -> [Text]
formatDescription d = ["=== Issue Description ===", d, ""]

formatCustomer :: Maybe Text -> Maybe Text -> [Text]
formatCustomer Nothing Nothing = []
formatCustomer mbName mbPhone =
  [ "=== Customer ===",
    "Name: " <> fromMaybe "N/A" mbName,
    "Phone: " <> fromMaybe "N/A" mbPhone,
    ""
  ]

formatMedia :: Maybe [Text] -> [Text]
formatMedia Nothing = []
formatMedia (Just []) = []
formatMedia (Just urls) =
  "=== Recording / Media ===" : map ("- " <>) urls

formatRide :: IT.RideInfo -> [Text]
formatRide IT.RideInfo {..}
  | T.null rideShortId = []
  | otherwise =
    catMaybes
      [ Just "=== Ride Info ===",
        Just $ "Ride ID: " <> rideShortId,
        Just $ "City: " <> rideCity,
        Just $ "Status: " <> status,
        Just $ "Vehicle: " <> vehicleNo,
        Just $ "Vehicle Category: " <> fromMaybe "N/A" vehicleCategory,
        Just $ "Created At: " <> showTimeIst rideCreatedAt,
        (\f -> "Fare: " <> show f) <$> fare,
        Just "",
        Just "=== Driver ===",
        Just $ "Name: " <> fromMaybe "N/A" driverName,
        Just $ "Phone: " <> fromMaybe "N/A" driverPhoneNo,
        Just ""
      ]

formatUpdateIssueDetails :: IT.UpdateIssueDetails -> [Text]
formatUpdateIssueDetails IT.UpdateIssueDetails {..} =
  maybe [] (\d -> ["", d]) issueDescription
    <> formatMedia mediaFiles

-- | Sender name + phone from @rideDescription@ — prefer customer-side values,
-- fall back to driver-side. Returns @(Nothing, Nothing)@ when no ride is
-- attached to the update (caller passes those into the multipart inputs and
-- 'buildUpdateBody' so the agent still sees the name in the message body).
senderInfoFromRide :: Maybe IT.RideInfo -> (Maybe Text, Maybe Text)
senderInfoFromRide Nothing = (Nothing, Nothing)
senderInfoFromRide (Just IT.RideInfo {..}) =
  ( firstJust customerName driverName,
    firstJust customerPhoneNo driverPhoneNo
  )
  where
    firstJust (Just a) _ = Just a
    firstJust Nothing b = b

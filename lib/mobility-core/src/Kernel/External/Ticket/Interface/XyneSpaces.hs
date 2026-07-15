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
-- Sender attribution: both 'createTicket' and 'updateTicket' use
-- @req.name@ / @req.phoneNo@ off the request record. When those are
-- absent on an update, we fall back to values derived from
-- @rideDescription@ (preferring customer over driver). Email is left
-- @Nothing@ for now — 'IssueManagement.Common.Person' has no email field.
--
-- Note on the returned @ticketId@: we echo back the threadId (= IssueReport id)
-- rather than Xyne's opaque ticketId so that the existing IssueManagement
-- caller can pass it back unchanged on subsequent updateTicket calls. Xyne's
-- own ticketId / xyneId / conversationId are logged but not propagated.
module Kernel.External.Ticket.Interface.XyneSpaces
  ( createTicket,
    updateTicket,
    updateTicketStatus,
    updateTicketCsat,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception as Exc
import qualified Data.Aeson as A
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
  let metadata = buildCreateMetadata req
      xyneReq =
        Xyne.XyneInboundReq
          { channelId = config.channelId,
            threadId = threadId,
            subject = buildSubject req.category req.rideDescription,
            -- Body carries only the customer's own message; everything else
            -- (category, ride info, phone numbers, media URLs) is surfaced to
            -- the Xyne agent via 'additionalFormFields'.
            body = req.issueDescription,
            externalId = Nothing,
            senderName = req.name,
            senderEmail = Nothing,
            additionalFormFields = if Map.null metadata then Nothing else Just metadata
          }
  -- One-time diagnostic: echo the exact JSON we hand to servant so we can
  -- verify @additionalFormFields@ actually lands on the wire. Remove once
  -- Xyne confirms it sees the field.
  logInfo $ "Xyne createTicket payload: " <> TE.decodeUtf8 (LBS.toStrict (A.encode xyneReq))
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
  let (mbRideName, mbRidePhone) = senderInfoFromRide req.rideDescription
      mbSenderName = req.name <|> mbRideName
      mbSenderPhone = req.phoneNo <|> mbRidePhone
  let xyneReq =
        Xyne.XyneInboundReq
          { channelId = config.channelId,
            threadId = threadId,
            subject = buildUpdateSubject req,
            body = buildUpdateBody req mbSenderName mbSenderPhone,
            externalId = Nothing,
            senderName = mbSenderName,
            senderEmail = Nothing,
            -- Updates carry only the chat comment for now; the metadata side
            -- panel was populated at create time and Xyne persists it against
            -- the threadId.
            additionalFormFields = Nothing
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

-- | Status-only update via Xyne's @/api/apps/ticket/updateTicket@. Deliberately
-- decoupled from 'updateTicket' (which posts a message via @appDeskInbound@):
-- the domain layer decides which of the two calls it needs, so a chat comment
-- does not re-sync status on every message and a status change does not have
-- to piggyback a fake comment.
--
-- @xyneTicketId@ must be Xyne's opaque id (as returned in @resp.ticketId@
-- from an inbound call), not our @threadId@.
updateTicketStatus ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  XyneSpacesCfg ->
  Text ->
  IT.TicketStatus ->
  m ()
updateTicketStatus config xyneTicketId status = do
  token <- decrypt config.token
  let statusReq =
        Xyne.XyneUpdateTicketReq
          { ticketId = xyneTicketId,
            channelId = config.channelId,
            statusV2 = ticketStatusToXyneV2 status
          }
  _ <- XF.updateTicketStatusAPI config.url token statusReq
  logInfo $ "Xyne updateTicketStatus synced: xyneTicketId=" <> xyneTicketId <> " statusV2=" <> statusReq.statusV2

-- | CSAT submission via Xyne's @/api/csat/external/:ticketId@. Decoupled from
-- 'updateTicket' / 'updateTicketStatus' the same way those two are decoupled
-- from each other — a satisfaction rating is a distinct event from a chat
-- comment or a status change. No-op (logs and returns @()@) when 'csatApiKey'
-- is not configured for this merchant, mirroring how other XyneSpaces-only
-- capabilities degrade for Kapture/Zendesk in the top-level 'Interface'.
--
-- @xyneTicketId@ must be Xyne's opaque id (as returned in @resp.ticketId@
-- from an inbound call), not our @threadId@.
updateTicketCsat ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  XyneSpacesCfg ->
  IT.UpdateTicketCsatReq ->
  m ()
updateTicketCsat config req = case config.csatApiKey of
  Nothing -> logInfo $ "Xyne updateTicketCsat skipped (no csatApiKey configured): xyneTicketId=" <> req.xyneTicketId
  Just encApiKey -> do
    apiKey <- decrypt encApiKey
    let csatReq =
          Xyne.XyneCsatReq
            { rating = req.rating,
              score = req.score,
              comment = req.comment
            }
    _ <- XF.updateCsatAPI config.url apiKey req.xyneTicketId csatReq
    logInfo $ "Xyne updateTicketCsat synced: xyneTicketId=" <> req.xyneTicketId <> " rating=" <> req.rating

-- | Map the shared 'IT.TicketStatus' enum onto Xyne's @statusV2@ labels.
-- Xyne's accepted set (per the documented API example): OPEN, IN_PROGRESS,
-- RESOLVED, CLOSED, REOPENED. Adjust here if Xyne's server-side Zod schema
-- rejects any of these values.
ticketStatusToXyneV2 :: IT.TicketStatus -> Text
ticketStatusToXyneV2 = \case
  IT.Open -> "OPEN"
  IT.Pending -> "IN_PROGRESS"
  IT.Solved -> "RESOLVED"
  IT.Closed -> "CLOSED"
  IT.Reopened -> "REOPENED"

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

-- | Resolve a media reference into @(formName, displayName, mimeType, path)@
-- suitable for 'appDeskInboundMultipartAPI', writing the bytes to a fresh
-- temp file under @/tmp@.
--
-- Two supported reference shapes:
--
--   * @data:[<mime>];base64,<content>@ — the caller has already fetched the
--     file (typically via 'AWS.S3.get') and embedded the base64 payload.
--     Preferred because it avoids re-fetching authenticated endpoints like
--     rider-app's @/v2/issue/media@, which requires the user's token that
--     rider-app itself doesn't hold when forwarding to Xyne.
--   * @http(s)://…@ — falls back to an HTTP GET. If the endpoint returns
--     @application/json@ (rider-app's own @IssueFetchMediaAPI@ is
--     @Get '[JSON] Text@ = @"<base64>"@), the body is JSON-decoded and
--     base64-decoded before being uploaded. Any other Content-Type is
--     treated as raw bytes.
--
-- In both cases, the real MIME type is re-derived from the decoded content's
-- magic bytes (PNG, JPEG, GIF, WebP, PDF, MP3, MP4, RIFF/WAV, OGG, HEIF/HEIC),
-- so an incorrect or absent MIME on the reference doesn't ruin the upload.
fetchToTempFile :: Text -> IO (Text, Text, Text, FilePath)
fetchToTempFile url = do
  (fileBytes, contentType) <-
    if "data:" `T.isPrefixOf` url
      then case decodeDataUri url of
        Right (mime, bs) -> pure (bs, mime)
        Left err ->
          Exc.throwIO . userError $
            "Xyne fetchToTempFile: could not decode data URI: " <> err
      else do
        mgr <- HCT.newTlsManager
        req <- HC.parseRequest (T.unpack url)
        resp <- HC.httpLbs req mgr
        let rawBody = HC.responseBody resp
            ct = responseContentType resp
            isJsonResponse = "application/json" `T.isPrefixOf` T.toLower ct
        bs <-
          if isJsonResponse
            then case decodeJsonBase64 rawBody of
              Right bs -> pure bs
              Left err ->
                Exc.throwIO . userError $
                  "Xyne fetchToTempFile: expected JSON-encoded base64 body from "
                    <> T.unpack url
                    <> " but decode failed: "
                    <> err
            else pure (LBS.toStrict rawBody)
        pure (bs, ct)
  let (mimeType, ext) = detectMimeAndExt fileBytes contentType
      displayName = deriveFilename url ext
  (path, hdl) <- openBinaryTempFile "/tmp" "xyne-attach-"
  BS.hPut hdl fileBytes
  hClose hdl
  pure ("files", displayName, mimeType, path)

-- | Parse a @data:[<mime>][;base64],<content>@ URI. Only base64-encoded
-- payloads are supported (that is what rider-app produces via 'AWS.S3.get').
decodeDataUri :: Text -> Either String (Text, BS.ByteString)
decodeDataUri url = do
  rest <- maybe (Left "not a data URI") Right $ T.stripPrefix "data:" url
  (header, content) <- case T.breakOn "," rest of
    (h, c) | not (T.null c) -> Right (h, T.drop 1 c)
    _ -> Left "missing comma in data URI"
  unless (";base64" `T.isInfixOf` (";" <> header)) $
    Left "data URI is not base64-encoded"
  let mime =
        let mimeCandidate = T.takeWhile (/= ';') header
         in if T.null mimeCandidate then "application/octet-stream" else mimeCandidate
  bs <- BA.convertFromBase BA.Base64 (TE.encodeUtf8 content)
  Right (mime, bs)

-- | Lower-cased Content-Type header value with @;charset=…@ / boundary params
-- stripped. Empty when the header is absent.
responseContentType :: HC.Response a -> Text
responseContentType resp =
  case lookup "Content-Type" (HC.responseHeaders resp) of
    Nothing -> ""
    Just bs -> T.strip . T.takeWhile (/= ';') . TE.decodeUtf8With (\_ _ -> Just '?') $ bs

-- | Parse an @application/json@ body of the form @"<base64>"@ (JSON string)
-- and decode to the raw bytes.
decodeJsonBase64 :: LBS.ByteString -> Either String BS.ByteString
decodeJsonBase64 lbs = do
  txt <- A.eitherDecode @Text lbs
  BA.convertFromBase BA.Base64 (TE.encodeUtf8 txt)

-- | Recognise the file type from its leading magic bytes. Falls back to the
-- provided response Content-Type (already stripped of params) and finally to
-- @application/octet-stream@ so Xyne at least gets a sensible upload.
detectMimeAndExt :: BS.ByteString -> Text -> (Text, Text)
detectMimeAndExt bs ct
  | BS.take 8 bs == BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] = ("image/png", "png")
  | BS.take 3 bs == BS.pack [0xFF, 0xD8, 0xFF] = ("image/jpeg", "jpg")
  | BS.take 6 bs `elem` [BS.pack [0x47, 0x49, 0x46, 0x38, 0x37, 0x61], BS.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61]] = ("image/gif", "gif")
  | BS.take 4 bs == BS.pack [0x52, 0x49, 0x46, 0x46] && BS.take 4 (BS.drop 8 bs) == BS.pack [0x57, 0x45, 0x42, 0x50] = ("image/webp", "webp")
  | BS.take 4 bs == BS.pack [0x52, 0x49, 0x46, 0x46] && BS.take 4 (BS.drop 8 bs) == BS.pack [0x57, 0x41, 0x56, 0x45] = ("audio/wav", "wav")
  | BS.take 4 bs == BS.pack [0x25, 0x50, 0x44, 0x46] = ("application/pdf", "pdf")
  | BS.take 3 bs == BS.pack [0x49, 0x44, 0x33] || BS.take 2 bs == BS.pack [0xFF, 0xFB] = ("audio/mpeg", "mp3")
  | BS.take 4 bs == BS.pack [0x4F, 0x67, 0x67, 0x53] = ("audio/ogg", "ogg")
  -- ISO Base Media File Format: bytes 4..8 = "ftyp"
  | BS.take 4 (BS.drop 4 bs) == BS.pack [0x66, 0x74, 0x79, 0x70] =
    let brand = BS.take 4 (BS.drop 8 bs)
     in if brand `elem` [BS.pack [0x68, 0x65, 0x69, 0x63], BS.pack [0x68, 0x65, 0x69, 0x78], BS.pack [0x6D, 0x69, 0x66, 0x31]]
          then ("image/heic", "heic")
          else
            if brand == BS.pack [0x4D, 0x34, 0x41, 0x20]
              then ("audio/mp4", "m4a")
              else ("video/mp4", "mp4")
  | not (T.null ct) && ct /= "application/json" = (ct, extFromMime ct)
  | otherwise = ("application/octet-stream", "bin")
  where
    extFromMime m = case m of
      "image/png" -> "png"
      "image/jpeg" -> "jpg"
      "image/gif" -> "gif"
      "image/webp" -> "webp"
      "image/heic" -> "heic"
      "application/pdf" -> "pdf"
      "audio/mpeg" -> "mp3"
      "audio/mp4" -> "m4a"
      "audio/wav" -> "wav"
      "audio/ogg" -> "ogg"
      "video/mp4" -> "mp4"
      "video/webm" -> "webm"
      _ -> "bin"

deriveFilename :: Text -> Text -> Text
deriveFilename url ext =
  let base = case fromQueryFilePath url of
        Just fp -> lastPathSegment fp
        Nothing -> lastPathSegment (T.takeWhile (/= '?') url)
      cleaned = if T.null base then "attachment" else base
      hasExt = T.isInfixOf "." cleaned
   in if hasExt then cleaned else cleaned <> "." <> ext

-- | Extract a @filePath=…@ query-string value if present. Rider-app's media
-- URL is shaped like @…/media?filePath=issue-media/customer-<id>/xxx.png@,
-- so this recovers the original object key that carries the extension.
fromQueryFilePath :: Text -> Maybe Text
fromQueryFilePath url =
  case T.breakOn "?" url of
    (_, qsWithMark)
      | not (T.null qsWithMark) ->
        let qs = T.drop 1 qsWithMark
            pairs = T.splitOn "&" qs
         in listToMaybe
              [ T.drop (T.length "filePath=") p
                | p <- pairs,
                  "filePath=" `T.isPrefixOf` p
              ]
    _ -> Nothing

lastPathSegment :: Text -> Text
lastPathSegment path =
  let parts = filter (not . T.null) (T.splitOn "/" path)
   in case reverse parts of
        (l : _) -> l
        [] -> ""

buildSubject :: Text -> Maybe IT.RideInfo -> Text
buildSubject category mbRide =
  category <> maybe "" (\r -> " - " <> r.rideShortId) mbRide

buildUpdateSubject :: IT.UpdateTicketReq -> Text
buildUpdateSubject req =
  let prefix = case req.issueDetails >>= (.category) of
        Just c -> c
        Nothing -> "Issue update"
   in prefix <> maybe "" (\r -> " - " <> r.rideShortId) req.rideDescription

-- | Build the metadata dict shown to the Xyne agent alongside the ticket.
-- Only non-empty values land in the map — Xyne's server-side Zod schema
-- rejects the whole request with @VALIDATION_ERROR@ when any value in
-- @additionalFormFields@ is @null@ or an empty string, so we drop empty
-- 'Maybe Text' fields (e.g. @Just ""@) here rather than shipping them.
-- @data:@-URI attachments are excluded (they arrive as multipart file
-- parts), while hosted @http(s)@ URLs are joined into a single
-- comma-separated line so agents can click through without the map
-- ballooning.
buildCreateMetadata :: IT.CreateTicketReq -> Map.Map Text Text
buildCreateMetadata IT.CreateTicketReq {..} =
  Map.fromList $
    catMaybes $
      [ nonEmpty "Category" category,
        subCategory >>= nonEmpty "Sub Category",
        name >>= nonEmpty "Customer Name",
        phoneNo >>= nonEmpty "Customer Phone Number"
      ]
        <> maybe [] rideFields rideDescription
        <> mediaField mediaFiles
  where
    rideFields r =
      [ nonEmpty "Ride ID" r.rideShortId,
        nonEmpty "Ride City" r.rideCity,
        nonEmpty "Ride Status" r.status,
        nonEmpty "Vehicle Number" r.vehicleNo,
        r.vehicleCategory >>= nonEmpty "Vehicle Category",
        r.vehicleServiceTier >>= nonEmpty "Vehicle Service Tier",
        Just ("Ride Created At", showTimeIst r.rideCreatedAt),
        (\f -> ("Fare", T.pack $ show f)) <$> r.fare,
        r.driverName >>= nonEmpty "Driver Name",
        r.driverPhoneNo >>= nonEmpty "Driver Phone Number"
      ]
    -- Xyne rejects both @""@ and whitespace-only strings (e.g. @" "@) with
    -- @VALIDATION_ERROR@. Trim first, then drop if empty; keep the trimmed
    -- value on the way out so we never ship stray padding either.
    nonEmpty k v =
      let stripped = T.strip v
       in if T.null stripped then Nothing else Just (k, stripped)
    mediaField Nothing = []
    mediaField (Just urls) =
      -- Drop base64 data URIs (they travel as multipart file parts). Only
      -- hosted URLs are useful in the metadata panel.
      let hosted = filter (not . ("data:" `T.isPrefixOf`)) urls
       in if null hosted then [] else [Just ("Media URLs", T.intercalate ", " hosted)]

buildUpdateBody :: IT.UpdateTicketReq -> Maybe Text -> Maybe Text -> Text
buildUpdateBody IT.UpdateTicketReq {..} mbName mbPhone =
  T.unlines $
    [comment]
      <> formatCustomer mbName mbPhone
      <> maybe [] formatRide rideDescription
      <> maybe [] formatUpdateIssueDetails issueDetails

formatCustomer :: Maybe Text -> Maybe Text -> [Text]
formatCustomer Nothing Nothing = []
formatCustomer mbName mbPhone =
  [ "=== Customer ===",
    "Name: " <> fromMaybe "N/A" mbName,
    "Phone: " <> fromMaybe "N/A" mbPhone,
    ""
  ]

-- | Renders the "Recording / Media" section of the body. Regular @http(s)@
-- URLs are inlined as text so agents can click through. @data:@ URIs, on the
-- other hand, are the multipart attachments themselves — their base64 payload
-- is many hundreds of KB, so inlining them inflates the @body@ form field
-- past Xyne's Zod cap ("Field value too long", HTTP 500). We keep only the
-- count for those; the actual bytes travel via the repeated @files@ parts.
formatMedia :: Maybe [Text] -> [Text]
formatMedia Nothing = []
formatMedia (Just []) = []
formatMedia (Just urls) =
  let (dataUris, httpUrls) = partition ("data:" `T.isPrefixOf`) urls
      httpLines = map ("- " <>) httpUrls
      attachmentLine = case length dataUris of
        0 -> []
        n -> ["- (" <> show n <> " file(s) uploaded as attachments)"]
      body = httpLines <> attachmentLine
   in if null body then [] else "=== Recording / Media ===" : body

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

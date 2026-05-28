{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Interface.Zendesk
  ( createTicket,
    updateTicket,
  )
where

import qualified Data.Text as T
import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as IT
import Kernel.External.Ticket.Zendesk.Config
import qualified Kernel.External.Ticket.Zendesk.Flow as ZF
import qualified Kernel.External.Ticket.Zendesk.Types as Zendesk
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Servant.Client
import Kernel.Utils.Time (showTimeIst)

createTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  ZendeskCfg ->
  IT.CreateTicketReq ->
  m IT.CreateTicketResp
createTicket config req = do
  apiKey <- decrypt config.apiKey
  let zendeskReq = mkZendeskCreateTicketReq config req
  resp <- ZF.createTicketAPI config.url apiKey zendeskReq
  pure
    IT.CreateTicketResp
      { ticketId = show resp.ticket.id,
        status = zendeskStatusToTicketStatus (fromMaybe "new" resp.ticket.status)
      }

mkZendeskCreateTicketReq :: ZendeskCfg -> IT.CreateTicketReq -> Zendesk.ZendeskCreateTicketReq
mkZendeskCreateTicketReq cfg IT.CreateTicketReq {..} =
  Zendesk.ZendeskCreateTicketReq
    { ticket =
        Zendesk.ZendeskTicketBody
          { subject = buildSubject category rideDescription,
            comment =
              Zendesk.ZendeskComment
                { body = buildBody issueDescription name phoneNo rideDescription mediaFiles,
                  htmlBody = buildHtmlBody issueDescription name phoneNo rideDescription mediaFiles
                },
            requester = Just $ Zendesk.ZendeskRequester {name = fromMaybe "Unknown" name, email = cfg.requesterEmail},
            organizationId = cfg.organizationId,
            groupId = cfg.groupId,
            priority = "normal",
            ticketType = "incident",
            customFields = buildCustomFields cfg rideDescription phoneNo
          }
    }

buildSubject :: Text -> Maybe IT.RideInfo -> Text
buildSubject category mbRide =
  category <> maybe "" (\r -> " - " <> r.rideShortId) mbRide

buildBody :: Text -> Maybe Text -> Maybe Text -> Maybe IT.RideInfo -> Maybe [Text] -> Text
buildBody issueDescription mbName mbPhone mbRide mbMediaFiles =
  issueDescription
    <> formatCustomerContext mbName mbPhone
    <> maybe "" formatRideInfo mbRide
    <> formatMediaFiles mbMediaFiles

formatCustomerContext :: Maybe Text -> Maybe Text -> Text
formatCustomerContext mbName mbPhone =
  T.unlines
    [ "",
      "=== Customer ===",
      "Name: " <> fromMaybe "N/A" mbName,
      "Phone: " <> fromMaybe "N/A" mbPhone
    ]

formatMediaFiles :: Maybe [Text] -> Text
formatMediaFiles Nothing = ""
formatMediaFiles (Just []) = ""
formatMediaFiles (Just urls) =
  T.unlines $ ["", "=== Recording / Media ==="] ++ map (\u -> "- " <> u) urls

formatMediaFilesHtml :: Maybe [Text] -> Text
formatMediaFilesHtml Nothing = ""
formatMediaFilesHtml (Just []) = ""
formatMediaFilesHtml (Just urls) =
  "<br><strong>Recording / Media</strong><ul>"
    <> T.concat (map (\u -> "<li><a href=\"" <> u <> "\">" <> extractLabel u <> "</a></li>") urls)
    <> "</ul>"
  where
    extractLabel url
      | "/rides/" `T.isInfixOf` url = "View Ride"
      | "/sos/" `T.isInfixOf` url = "View SOS Media"
      | otherwise =
        let withoutQuery = T.takeWhile (/= '?') url
            parts = T.splitOn "/" withoutQuery
         in case filter (not . T.null) parts of
              [] -> url
              ps -> last ps

htmlEscape :: Text -> Text
htmlEscape =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&#39;"

-- html_body is only set when media files are present.
-- It wraps the plain-text body in a <pre> tag (preserving formatting) and
-- appends the media links as proper HTML anchors.
-- Zendesk shows html_body in the agent UI and falls back to body elsewhere,
-- so agents see exactly one copy of the content.
buildHtmlBody :: Text -> Maybe Text -> Maybe Text -> Maybe IT.RideInfo -> Maybe [Text] -> Maybe Text
buildHtmlBody issueDescription mbName mbPhone mbRide mbMediaFiles =
  case formatMediaFilesHtml mbMediaFiles of
    "" -> Nothing
    mediaHtml ->
      Just $
        "<pre>" <> htmlEscape (buildBody issueDescription mbName mbPhone mbRide Nothing) <> "</pre>"
          <> mediaHtml

buildHtmlUpdateBody :: Text -> Maybe IT.RideInfo -> Maybe IT.UpdateIssueDetails -> Maybe Text
buildHtmlUpdateBody comment mbRide mbIssueDetails =
  let mediaHtml = maybe "" (formatMediaFilesHtml . (.mediaFiles)) mbIssueDetails
   in case mediaHtml of
        "" -> Nothing
        html ->
          Just $
            "<pre>" <> htmlEscape (buildUpdateBody comment mbRide (fmap (\d -> (d :: IT.UpdateIssueDetails) {IT.mediaFiles = Nothing}) mbIssueDetails)) <> "</pre>"
              <> html

formatRideInfo :: IT.RideInfo -> Text
formatRideInfo IT.RideInfo {..}
  | T.null rideShortId = ""
  | otherwise =
    T.unlines $
      catMaybes
        [ Just "",
          Just "=== Ride Info ===",
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
          Just "",
          Just "=== Pickup Location ===",
          Just $ formatLocation pickupLocation,
          Just "=== Drop Location ===",
          Just $ maybe "N/A" formatLocation dropLocation
        ]

formatLocation :: IT.Location -> Text
formatLocation IT.Location {..} =
  T.intercalate ", " $
    filter (not . T.null) $
      map
        (fromMaybe "")
        [ building,
          street,
          area,
          city,
          state,
          country
        ]
        ++ ["Lat: " <> show lat <> ", Lon: " <> show lon]

updateTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  ZendeskCfg ->
  IT.UpdateTicketReq ->
  m IT.UpdateTicketResp
updateTicket config req = do
  apiKey <- decrypt config.apiKey
  let zendeskReq = mkZendeskUpdateTicketReq req
  void $ ZF.updateTicketAPI config.url apiKey req.ticketId zendeskReq
  -- Return req.status directly: Zendesk echoes "open" for Reopened, so parsing
  -- the response would lose the caller's intent. Mirroring what Kapture does.
  pure
    IT.UpdateTicketResp
      { ticketId = req.ticketId,
        status = req.status,
        message = "Ticket updated"
      }

mkZendeskUpdateTicketReq :: IT.UpdateTicketReq -> Zendesk.ZendeskUpdateTicketReq
mkZendeskUpdateTicketReq IT.UpdateTicketReq {..} =
  Zendesk.ZendeskUpdateTicketReq
    { ticket =
        Zendesk.ZendeskUpdateTicketBody
          { comment =
              Just
                Zendesk.ZendeskComment
                  { body = buildUpdateBody comment rideDescription issueDetails,
                    htmlBody = buildHtmlUpdateBody comment rideDescription issueDetails
                  },
            status = Just $ ticketStatusToZendesk status
          }
    }

buildUpdateBody :: Text -> Maybe IT.RideInfo -> Maybe IT.UpdateIssueDetails -> Text
buildUpdateBody comment mbRide mbIssueDetails =
  comment
    <> maybe "" formatRideInfo mbRide
    <> maybe "" formatIssueDetails mbIssueDetails

formatIssueDetails :: IT.UpdateIssueDetails -> Text
formatIssueDetails IT.UpdateIssueDetails {..} =
  let mediaSection = formatMediaFiles mediaFiles
      descSection = maybe "" (\d -> "\n" <> d) issueDescription
   in descSection <> mediaSection

ticketStatusToZendesk :: IT.TicketStatus -> Text
ticketStatusToZendesk IT.Open = "open"
ticketStatusToZendesk IT.Pending = "pending"
ticketStatusToZendesk IT.Solved = "solved"
ticketStatusToZendesk IT.Closed = "closed"
-- Zendesk has no "reopened" status; sending "open" is the correct equivalent.
-- Round-trip is lossy by design: updateTicket returns req.status, not the parsed response.
ticketStatusToZendesk IT.Reopened = "open"

zendeskStatusToTicketStatus :: Text -> IT.TicketStatus
zendeskStatusToTicketStatus "open" = IT.Open
zendeskStatusToTicketStatus "new" = IT.Open
zendeskStatusToTicketStatus "hold" = IT.Pending
zendeskStatusToTicketStatus "pending" = IT.Pending
zendeskStatusToTicketStatus "solved" = IT.Solved
zendeskStatusToTicketStatus "closed" = IT.Closed
zendeskStatusToTicketStatus _ = IT.Open

buildCustomFields :: ZendeskCfg -> Maybe IT.RideInfo -> Maybe Text -> [Zendesk.ZendeskCustomField]
buildCustomFields cfg mbRide mbCustomerPhone =
  catMaybes
    [ mkField cfg.rideIdFieldId (toJSON . (.rideShortId) <$> mbRide),
      mkField cfg.driverPhoneFieldId (toJSON <$> (mbRide >>= (.driverPhoneNo))),
      mkField cfg.customerPhoneFieldId (toJSON <$> mbCustomerPhone)
    ]
  where
    mkField Nothing _ = Nothing
    mkField _ Nothing = Nothing
    mkField (Just fid) (Just val) = Just $ Zendesk.ZendeskCustomField {fieldId = fid, value = val}

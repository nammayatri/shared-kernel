{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Interface.Kapture
  ( createTicket,
    updateTicket,
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as IT
import Kernel.External.Ticket.Kapture.Config
import qualified Kernel.External.Ticket.Kapture.Flow as KF
import Kernel.External.Ticket.Kapture.Types as Kapture
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics

createTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  KaptureCfg ->
  IT.CreateTicketReq ->
  m Kapture.CreateTicketResp
createTicket config req = do
  auth <- decrypt config.auth
  KF.createTicketAPI config.url config.version auth (mkCreateTicketReq req)

mkCreateTicketReq :: IT.CreateTicketReq -> Kapture.CreateTicketReq
mkCreateTicketReq IT.CreateTicketReq {..} =
  Kapture.CreateTicketReq
    { title = category,
      ticketDetails = issueDescription,
      disposition,
      queue,
      customerId = personId,
      customerName = name,
      phone = phoneNo,
      issueDetails = mkIssueDetails,
      rideDetails = mkRideDescriptionDriver <$> rideDescription,
      classification = classification
    }
  where
    mkIssueDetails =
      Kapture.IssueDetails {..}

mkRideDescriptionDriver :: IT.RideInfo -> Kapture.RideInfo
mkRideDescriptionDriver IT.RideInfo {..} =
  Kapture.RideInfo
    { pickupLocationLat = Just pickupLocation.lat,
      pickupLocationLon = Just pickupLocation.lon,
      pickupLocationStreet = pickupLocation.street,
      pickupLocationCity = pickupLocation.city,
      pickupLocationState = pickupLocation.state,
      pickupLocationCountry = pickupLocation.country,
      pickupLocationBuilding = pickupLocation.building,
      pickupLocationArea = pickupLocation.area,
      dropLocationLat = (.lat) <$> dropLocation,
      dropLocationLon = (.lon) <$> dropLocation,
      dropLocationStreet = (.street) =<< dropLocation,
      dropLocationCity = (.city) =<< dropLocation,
      dropLocationState = (.state) =<< dropLocation,
      dropLocationCountry = (.country) =<< dropLocation,
      dropLocationBuilding = (.building) =<< dropLocation,
      dropLocationArea = (.area) =<< dropLocation,
      ..
    }

updateTicket ::
  ( Metrics.CoreMetrics m,
    EncFlow m r
  ) =>
  KaptureCfg ->
  IT.UpdateTicketReq ->
  m Kapture.UpdateTicketResp
updateTicket config req = do
  auth <- decrypt config.auth
  KF.updateTicketAPI config.url config.version auth (mkUpdateTicketReq req)

mkUpdateTicketReq :: IT.UpdateTicketReq -> Kapture.UpdateTicketReq
mkUpdateTicketReq IT.UpdateTicketReq {..} =
  Kapture.UpdateTicketReq
    { comment = comment,
      ticket_id = ticketId,
      sub_status = show subStatus
    }

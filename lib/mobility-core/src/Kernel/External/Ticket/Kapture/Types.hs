{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Ticket.Kapture.Types
  ( module Kernel.External.Ticket.Kapture.Types,
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (Money)
import Kernel.Utils.JSON

data Classification = DRIVER | CUSTOMER
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateTicketReq = CreateTicketReq
  { title :: Text,
    ticketDetails :: Text,
    disposition :: Text,
    queue :: Text,
    customerId :: Text,
    customerName :: Maybe Text,
    phone :: Maybe Text,
    issueDetails :: IssueDetails,
    rideDetails :: Maybe RideInfo,
    classification :: Classification
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CreateTicketReq where
  toJSON = genericToJSON constructorsWithSnakeCase

data UpdateTicketReq = UpdateTicketReq
  { comment :: Text,
    ticket_id :: Text,
    sub_status :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON UpdateTicketReq where
  toJSON = genericToJSON constructorsWithSnakeCase

data IssueDetails = IssueDetails
  { issueDescription :: Text,
    issueId :: Maybe Text,
    mediaFiles :: Maybe [Text],
    subCategory :: Maybe Text,
    category :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON IssueDetails where
  toJSON = genericToJSON constructorsWithSnakeCase

data RideInfo = RideInfo
  { rideShortId :: Text,
    rideCity :: Text,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Maybe Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    status :: Text,
    rideCreatedAt :: UTCTime,
    vehicleCategory :: Maybe Text,
    vehicleServiceTier :: Maybe Text,
    pickupLocationLat :: Maybe Double,
    pickupLocationLon :: Maybe Double,
    pickupLocationStreet :: Maybe Text,
    pickupLocationCity :: Maybe Text,
    pickupLocationState :: Maybe Text,
    pickupLocationCountry :: Maybe Text,
    pickupLocationBuilding :: Maybe Text,
    pickupLocationArea :: Maybe Text,
    dropLocationLat :: Maybe Double,
    dropLocationLon :: Maybe Double,
    dropLocationStreet :: Maybe Text,
    dropLocationCity :: Maybe Text,
    dropLocationState :: Maybe Text,
    dropLocationCountry :: Maybe Text,
    dropLocationBuilding :: Maybe Text,
    dropLocationArea :: Maybe Text,
    fare :: Maybe Money
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase

data CreateTicketResp = CreateTicketResp
  { additionalDetails :: AdditionalDetails,
    ticketId :: Text,
    ticket :: TicketDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "additionalDetails" -> "Additional_details"
        "ticketId" -> "ticket_id"
        other -> other
    }

instance FromJSON CreateTicketResp where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateTicketResp where
  toJSON = genericToJSON jsonOptions

data UpdateTicketResp = UpdateTicketResp
  { message :: Text,
    status :: Text,
    ticket :: TicketDetails
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON UpdateTicketResp where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateTicketResp where
  toJSON = genericToJSON jsonOptions

newtype AdditionalDetails = AdditionalDetails
  { issueDetails :: [IssueResp]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdditionalDetails where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON AdditionalDetails where
  toJSON = genericToJSON constructorsWithSnakeCase

newtype IssueResp = IssueResp
  { issueId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON IssueResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON IssueResp where
  toJSON = genericToJSON constructorsWithSnakeCase

newtype TicketDetails = TicketDetails
  { subStatus :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON TicketDetails where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON TicketDetails where
  toJSON = genericToJSON constructorsWithLowerCase

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Interface.Types
  ( module Reexport,
    module Kernel.External.Ticket.Interface.Types,
  )
where

import Deriving.Aeson
import Kernel.External.Ticket.Kapture.Config as Kapture
import Kernel.External.Ticket.Kapture.Types as Reexport (Classification (..), CreateTicketResp (..), UpdateTicketResp (..))
import Kernel.External.Ticket.Types as Reexport
import Kernel.Prelude
import Kernel.Types.Common (Money)
import Kernel.Types.HideSecrets

newtype IssueTicketServiceConfig = KaptureConfig Kapture.KaptureCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateTicketReq = CreateTicketReq
  { category :: Text,
    subCategory :: Maybe Text,
    disposition :: Text,
    queue :: Text,
    issueId :: Maybe Text,
    issueDescription :: Text,
    mediaFiles :: Maybe [Text],
    name :: Maybe Text,
    phoneNo :: Maybe Text,
    personId :: Text,
    classification :: Classification,
    rideDescription :: Maybe RideInfo,
    becknIssueId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateTicketReq where
  hideSecrets = identity

data RideInfo = RideInfo
  { rideShortId :: Text,
    rideCity :: Text,
    vehicleCategory :: Maybe Text,
    vehicleServiceTier :: Maybe Text,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Maybe Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    status :: Text,
    rideCreatedAt :: UTCTime,
    pickupLocation :: Location,
    dropLocation :: Maybe Location,
    fare :: Maybe Money
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data Location = Location
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UpdateTicketReq = UpdateTicketReq
  { comment :: Text,
    ticketId :: Text,
    subStatus :: SubStatus
  }

data SubStatus = OP | IN | RS | PE | CL | CRS
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- SubStatusName	  SubStatus Key
-- OPEN	            OP
-- PENDING INTERNAL	IN
-- RESOLVED	        RS
-- PENDING EXTERNAL	PE
-- CLOSED	          CL
-- REOPENED         CRS

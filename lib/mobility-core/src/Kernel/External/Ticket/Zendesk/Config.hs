{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.Zendesk.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data ZendeskCfg = ZendeskCfg
  { url :: BaseUrl,
    -- apiKey must be stored as pre-encoded Base64 of "email/token:api_token" (Zendesk Basic Auth format).
    -- The value is sent directly as "Basic <apiKey>" in the Authorization header.
    apiKey :: EncryptedField 'AsEncrypted Text,
    requesterEmail :: Maybe Text,
    organizationId :: Maybe Int,
    groupId :: Maybe Int,
    sosGroupId :: Maybe Int,
    feedbackGroupId :: Maybe Int,
    formId :: Maybe Int,
    sosFormId :: Maybe Int,
    rideIdFieldId :: Maybe Int,
    driverPhoneFieldId :: Maybe Int,
    customerPhoneFieldId :: Maybe Int,
    cityFieldId :: Maybe Int,
    vehicleCategoryFieldId :: Maybe Int,
    ticketContextFieldId :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Ticket.XyneSpaces.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data XyneSpacesCfg = XyneSpacesCfg
  { -- | Xyne API base URL, e.g. https://api.your-xyne-host.com
    url :: BaseUrl,
    -- | Shared JWT sent as @Authorization: Bearer <token>@ on every outbound request.
    token :: EncryptedField 'AsEncrypted Text,
    -- | The desk channel this merchant talks to.
    channelId :: Text,
    -- | Fallback sender id used on inbound DESK_REPLY webhooks when the payload
    -- omits @replierUserId@. Treated as an opaque @Id User@ — a real dashboard
    -- user row is not required.
    xyneAgentUserId :: Text,
    -- | Strip HTML from inbound reply bodies before persisting as the chat
    -- @message@ field. Defaults to True on read when omitted from JSON.
    convertHtmlToPlainText :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
    convertHtmlToPlainText :: Maybe Bool,
    -- | @X-Api-Key@ sent on CSAT submission calls (@/api/csat/external/:ticketId@).
    -- Separate credential from 'token' (Bearer JWT) since Xyne issues and
    -- rotates the two independently. 'Nothing' when CSAT push is not configured.
    csatApiKey :: Maybe (EncryptedField 'AsEncrypted Text),
    -- | HMAC-SHA256 secret used to verify the @X-Xyne-Signature@ on inbound
    -- @DESK_REPLY@ webhooks. Xyne issues one per Xyne app, and a deployment
    -- has only one webhook URL, so the calling app is named by the @mocId@
    -- query param on that URL and its secret read from here. 'Nothing' falls
    -- back to the deployment-wide @xyneWebhookSigningSecret@, which is what
    -- every merchant used before separate Xyne apps existed.
    --
    -- Encrypted like 'token' and 'csatApiKey': it is a credential at rest, so
    -- it must not be readable from the config row or from a 'Show' of this
    -- type. Consequence for operators: the ciphertext comes from passetto, so
    -- the value cannot be written with plain SQL.
    webhookSigningSecret :: Maybe (EncryptedField 'AsEncrypted Text),
    -- | Xyne workflow that drives the pre-ticket support bot
    -- (@POST /sdlc-api/apps/workflows/conversations@). 'Nothing' disables the bot
    -- proxy for this merchant.
    botWorkflowId :: Maybe Text,
    -- | Optional app token for the bot conversation API when it is issued for
    -- a different Xyne app than 'token'. Falls back to 'token' when absent.
    botToken :: Maybe (EncryptedField 'AsEncrypted Text)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

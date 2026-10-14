{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.Breeze.Config where

import Kernel.External.Encryption
import Kernel.Prelude

-- | Configuration for the BreezeIQ server-side event collector.
--
-- @tenantId@ and @workspaceId@ are sent both as headers and in each event body.
-- Event types are registered per tenant; the workspace decides which profile
-- pool the event lands in (an event without one goes to @default@).
data BreezeCfg = BreezeCfg
  { baseUrl :: BaseUrl,
    tenantId :: Text,
    workspaceId :: Text,
    collectorToken :: EncryptedField 'AsEncrypted Text,
    -- | Identifier type under which the request's @customerId@ is sent in
    -- @properties.identifiers@ (for example @driver_id@, once the tenant has
    -- declared it). Breeze ignores types the tenant has not declared, and drops
    -- an event with no usable identifier, so leave this unset only when callers
    -- put identifiers into the event attributes themselves.
    customerIdentifierType :: Maybe Text,
    enabled :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

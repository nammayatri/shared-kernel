{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.FleetEngine.Config where

import Kernel.External.Encryption
import Kernel.Prelude

-- | Per-merchant-operating-city Fleet Engine (On-demand Rides & Deliveries)
-- configuration. Modelled on 'Kernel.External.Maps.Google.Config.GoogleCfg':
-- the service-account JSON is stored encrypted and decrypted server-side only
-- (it is never shipped to apps; the backend mints short-lived scoped JWTs from
-- it). Absence of this config in a city is treated as "feature off" by callers.
data FleetEngineCfg = FleetEngineCfg
  { -- | GCP project id that owns the Fleet Engine provider (the @providers/{id}@ path segment)
    providerId :: Text,
    -- | Fleet Engine REST host; defaults to https://fleetengine.googleapis.com when unset
    fleetEngineUrl :: Maybe BaseUrl,
    -- | Service-account JSON (raw JSON text), encrypted at rest
    serviceAccountJson :: EncryptedField 'AsEncrypted Text,
    -- | TTL (seconds) for consumer (rider) SDK tokens; defaults to 'defaultConsumerTokenTtl'
    consumerTokenTtlSeconds :: Maybe Integer,
    -- | TTL (seconds) for driver SDK tokens; defaults to 'defaultDriverTokenTtl'
    driverTokenTtlSeconds :: Maybe Integer,
    -- | TTL (seconds) for server-to-server tokens; defaults to 'defaultServerTokenTtl'
    serverTokenTtlSeconds :: Maybe Integer
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

defaultConsumerTokenTtl :: Integer
defaultConsumerTokenTtl = 15 * 60

defaultDriverTokenTtl :: Integer
defaultDriverTokenTtl = 60 * 60

defaultServerTokenTtl :: Integer
defaultServerTokenTtl = 60 * 60

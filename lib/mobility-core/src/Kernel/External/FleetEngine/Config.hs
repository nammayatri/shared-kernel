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
--
-- Following Google's least-privilege guidance, each token role is signed by its
-- own service account (each granted only the matching Fleet Engine IAM role):
-- the server SA (@roles/fleetengine.ondemandAdmin@) for CreateTrip/UpdateTrip and
-- server-to-server tokens, the driver SA (@roles/fleetengine.driverSdkUser@) for
-- Driver SDK tokens, and the consumer SA (@roles/fleetengine.consumerSdkUser@)
-- for rider Consumer SDK tokens.
data FleetEngineCfg = FleetEngineCfg
  { -- | GCP project id that owns the Fleet Engine provider (the @providers/{id}@ path segment)
    providerId :: Text,
    -- | Kill switch. 'Nothing' or 'Just False' => integration off; callers should treat absence as off.
    enabled :: Maybe Bool,
    -- | Fleet Engine REST host; defaults to https://fleetengine.googleapis.com when unset
    fleetEngineUrl :: Maybe BaseUrl,
    -- | Server service-account JSON (ondemandAdmin role) for CreateTrip/UpdateTrip + server tokens, encrypted at rest
    serverServiceAccountJson :: EncryptedField 'AsEncrypted Text,
    -- | Driver service-account JSON (driverSdkUser role) for Driver SDK tokens, encrypted at rest
    driverServiceAccountJson :: EncryptedField 'AsEncrypted Text,
    -- | Consumer service-account JSON (consumerSdkUser role) for rider Consumer SDK tokens, encrypted at rest
    consumerServiceAccountJson :: EncryptedField 'AsEncrypted Text,
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

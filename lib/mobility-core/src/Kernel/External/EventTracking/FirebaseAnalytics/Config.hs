{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.EventTracking.FirebaseAnalytics.Config where

import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Version (DeviceType)

-- | GA4 Measurement Protocol config; one app id + secret per client platform.
data FirebaseAnalyticsCfg = FirebaseAnalyticsCfg
  { baseUrl :: BaseUrl,
    apps :: [FirebaseAppCfg],
    enabled :: Bool,
    -- | Send to @/debug/mp/collect@ and log validation messages instead of recording.
    debug :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FirebaseAppCfg = FirebaseAppCfg
  { platform :: DeviceType,
    firebaseAppId :: Text,
    apiSecret :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

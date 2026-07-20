{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Kernel.External.EventTracking.Interface.Types
  ( EventTrackingServiceConfig (..),
    EventTrackingReq (..),
  )
where

import Data.Aeson
import Deriving.Aeson
import qualified Kernel.External.EventTracking.Clevertap.Config as ClevertapConfig
import qualified Kernel.External.EventTracking.Moengage.Config as MoengageConfig
import Kernel.Prelude

-- | Configuration sum type for all event tracking providers
data EventTrackingServiceConfig
  = MoengageConfig MoengageConfig.MoengageCfg
  | ClevertapConfig ClevertapConfig.ClevertapCfg
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] EventTrackingServiceConfig

-- | Provider-agnostic event payload.
--
-- Callers build this; each provider's Flow maps it to that vendor's wire type.
-- Keeping it neutral means adding a provider does not touch calling services.
data EventTrackingReq = EventTrackingReq
  { customerId :: Text,
    eventName :: Text,
    attributes :: Value,
    -- | Event time, where the provider supports it. Moengage ignores this.
    timestamp :: Maybe UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

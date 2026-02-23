{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Plasma.LMS.Config
  ( LMSCfg (..),
  )
where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude

-- | LMS Service config
-- The BaseUrl should have baseUrlPath set to "/" (or empty)
-- The full API path is defined in the Servant API type
data LMSCfg = LMSCfg
  { url :: BaseUrl, -- Base URL: https://api.sandbox.moving.tech/dev/dobpp/ui/plasma -- master plasma url (with baseUrlPath = "/")
    apiKey :: EncryptedField 'AsEncrypted Text -- Encrypted API key for LMS API
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

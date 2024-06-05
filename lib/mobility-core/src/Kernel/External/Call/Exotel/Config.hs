{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Call.Exotel.Config
  ( ExotelCfg (..),
    ExotelApiKey (..),
    ExotelApiToken (..),
    ExotelAccountSID (..),
    ExotelCallerId (..),
  )
where

import Kernel.External.Call.Exotel.Types
import Kernel.External.Encryption
import Kernel.Prelude

-- | Exotel Service config
data ExotelCfg = ExotelCfg
  { exotelUrl :: BaseUrl,
    callbackUrl :: BaseUrl,
    url :: BaseUrl, -- applet url for Call Flow (IVR, greeting etc.)
    apiKey :: EncryptedField 'AsEncrypted ExotelApiKey,
    apiToken :: EncryptedField 'AsEncrypted ExotelApiToken,
    accountSID :: ExotelAccountSID,
    callerId :: ExotelCallerId
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

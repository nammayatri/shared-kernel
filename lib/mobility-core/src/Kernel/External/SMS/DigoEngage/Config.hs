{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SMS.DigoEngage.Config where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kernel.External.Encryption
import Kernel.External.SMS.DigoEngage.Types
import Kernel.Prelude (Eq, HasField (hasField), Show)
import Servant.Client (BaseUrl)

data DigoEngageSmsCfg = DigoEngageSmsCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    apiToken :: EncryptedField 'AsEncrypted Text,
    from :: Text,
    username :: Text,
    url :: BaseUrl,
    tiny :: Text,
    messageType :: Text,
    tlv :: TLVForSmsCfg,
    dlr :: ClientDomain
  }
  deriving (Show, Eq, Generic)

instance FromJSON DigoEngageSmsCfg where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageType" -> "type"
            other -> other
        }

instance ToJSON DigoEngageSmsCfg where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "messageType" -> "type"
            other -> other
        }

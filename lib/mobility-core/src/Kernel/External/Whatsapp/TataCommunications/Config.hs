{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Whatsapp.TataCommunications.Config where

import Data.Aeson (object, withObject, (.:), (.=))
import Kernel.External.Encryption
import Kernel.Prelude

-- Language sub-structure for templates
data Language = Language
  { code :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Template structure containing name and language
data Template = Template
  { name :: Text,
    language :: Language
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Main config structure
data TataCommunicationsCfg = TataCommunicationsCfg
  { apiToken :: EncryptedField 'AsEncrypted Text,
    url :: BaseUrl,
    from :: Text,
    type_ :: Text,
    template :: Template
  }
  deriving (Show, Eq, Generic)

-- Manual JSON decoding with custom field name mapping
instance FromJSON TataCommunicationsCfg where
  parseJSON = withObject "TataCommunicationsCfg" $ \v ->
    TataCommunicationsCfg
      <$> v .: "apiToken"
      <*> v .: "url"
      <*> v .: "from"
      <*> v .: "type"
      <*> v .: "template"

-- Manual JSON encoding with custom field name mapping
instance ToJSON TataCommunicationsCfg where
  toJSON TataCommunicationsCfg {..} =
    object
      [ "apiToken" .= apiToken,
        "url" .= url,
        "from" .= from,
        "type" .= type_,
        "template" .= template
      ]

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.CreateCustomer where

import Data.Aeson
import Kernel.Prelude

data CreateCustomerRequest = CreateCustomerRequest
  { object_reference_id :: Text,
    mobile_number :: Text,
    email_address :: Maybe Text,
    first_name :: Maybe Text,
    last_name :: Maybe Text,
    mobile_country_code :: Maybe Text,
    options_get_client_auth_token :: Maybe Bool
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

jsonOptionsCCR :: Options
jsonOptionsCCR =
  defaultOptions
    { fieldLabelModifier = \case
        "options_get_client_auth_token" -> "options.get_client_auth_token"
        other -> other
    }

instance FromJSON CreateCustomerRequest where
  parseJSON = genericParseJSON jsonOptionsCCR

instance ToJSON CreateCustomerRequest where
  toJSON = genericToJSON jsonOptionsCCR

data CreateCustomerResp = CreateCustomerResp
  { last_name :: Maybe Text,
    mobile_number :: Maybe Text,
    id :: Maybe Text,
    last_updated :: Maybe Text,
    mobile_country_code :: Maybe Text,
    date_created :: Maybe Text,
    email_address :: Maybe Text,
    object_reference_id :: Text,
    customer_object :: Maybe Text,
    first_name :: Maybe Text,
    juspay :: Maybe ClientAuthDetails
  }
  deriving stock (Show, Generic, Eq)

data ClientAuthDetails = ClientAuthDetails
  { client_auth_token :: Text,
    client_auth_token_expiry :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

jsonOptionsData :: Options
jsonOptionsData =
  defaultOptions
    { fieldLabelModifier = \case
        "customer_object" -> "object"
        other -> other
    }

instance FromJSON CreateCustomerResp where
  parseJSON = genericParseJSON jsonOptionsData

instance ToJSON CreateCustomerResp where
  toJSON = genericToJSON jsonOptionsData

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.Person where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Kernel.External.Payment.Stripe.Types.Accounts (Address (..), DateOfBirth (..), insertOrAppend)
import Kernel.Prelude
import Kernel.Utils.JSON
import Web.FormUrlEncoded
import Web.HttpApiData (ToHttpApiData (..))

data Relationship = Relationship
  { representative :: Maybe Bool,
    owner :: Maybe Bool,
    director :: Maybe Bool,
    executive :: Maybe Bool,
    percent_ownership :: Maybe Double,
    title :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PersonReq = PersonReq
  { first_name :: Maybe Text,
    last_name :: Maybe Text,
    email :: Maybe Text,
    phone :: Maybe Text,
    dob :: Maybe DateOfBirth,
    address :: Maybe Address,
    id_number :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToForm PersonReq where
  toForm PersonReq {..} =
    Form $
      foldl' insertOrAppend HM.empty $
        catMaybes
          [ ("first_name",) <$> toQueryParam <$> first_name,
            ("last_name",) <$> toQueryParam <$> last_name,
            ("email",) <$> toQueryParam <$> email,
            ("phone",) <$> toQueryParam <$> phone,
            ("id_number",) <$> toQueryParam <$> id_number
          ]
          ++ maybe [] personDobToForm dob
          ++ maybe [] personAddressToForm address

personDobToForm :: DateOfBirth -> [(Text, Text)]
personDobToForm DateOfBirth {..} =
  [ ("dob[day]", toQueryParam day),
    ("dob[month]", toQueryParam month),
    ("dob[year]", toQueryParam year)
  ]

personAddressToForm :: Address -> [(Text, Text)]
personAddressToForm Address {..} =
  catMaybes
    [ ("address[line1]",) <$> toQueryParam <$> line1,
      ("address[line2]",) <$> toQueryParam <$> line2,
      ("address[city]",) <$> toQueryParam <$> city,
      ("address[state]",) <$> toQueryParam <$> state,
      ("address[postal_code]",) <$> toQueryParam <$> postal_code,
      ("address[country]",) <$> toQueryParam <$> country
    ]

data PersonVerificationStatus
  = PersonVerificationStatusUnverified
  | PersonVerificationStatusPending
  | PersonVerificationStatusVerified
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

personVerificationStatusJsonOptions :: Options
personVerificationStatusJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "PersonVerificationStatusUnverified" -> "unverified"
        "PersonVerificationStatusPending" -> "pending"
        "PersonVerificationStatusVerified" -> "verified"
        x -> x
    }

instance FromJSON PersonVerificationStatus where
  parseJSON = genericParseJSON personVerificationStatusJsonOptions

instance ToJSON PersonVerificationStatus where
  toJSON = genericToJSON personVerificationStatusJsonOptions

data PersonVerification = PersonVerification
  { status :: PersonVerificationStatus,
    details :: Maybe Text,
    details_code :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PersonObject = PersonObject
  { id :: Text,
    first_name :: Maybe Text,
    last_name :: Maybe Text,
    relationship :: Maybe Relationship,
    verification :: Maybe PersonVerification
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PersonList = PersonList
  { _data :: [PersonObject],
    has_more :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON PersonList where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PersonList where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

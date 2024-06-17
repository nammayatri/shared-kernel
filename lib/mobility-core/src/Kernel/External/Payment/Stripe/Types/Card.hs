{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.Card where

import Data.Aeson
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Utils.JSON

data CardObject = CardObject
  { id :: PaymentMethodId,
    name :: Maybe Text,
    brand :: Text,
    country :: Maybe Text,
    customer :: Maybe Text,
    cvc_check :: Maybe CVCCheck,
    exp_month :: Int,
    exp_year :: Int,
    funding :: Maybe CardFunding,
    fingerprint :: Maybe Text,
    last4 :: Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CVCCheck = Pass | Fail | Unavailable | Unchecked
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON CVCCheck where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON CVCCheck where
  toJSON = genericToJSON constructorsWithLowerCase

data CardFunding = Credit | Debit | Prepaid | Unknown
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON CardFunding where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON CardFunding where
  toJSON = genericToJSON constructorsWithLowerCase

newtype CardReq = CardReq
  { source :: CardReqSource
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CardReqSource = CardReqSource
  { exp_month :: Int,
    exp_year :: Int,
    number :: Text,
    _object :: PaymentSourceType,
    cvc :: Text,
    name :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON CardReqSource where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CardReqSource where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data PaymentSourceType = Card | Wallet
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON PaymentSourceType where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON PaymentSourceType where
  toJSON = genericToJSON constructorsWithLowerCase

data UpdateCardReq = UpdateCardReq
  { exp_month :: Maybe Int,
    exp_year :: Maybe Int,
    name :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DeleteCardResp = DeleteCardResp
  { id :: PaymentMethodId,
    deleted :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CardList = CardList
  { _data :: [CardObject],
    has_more :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON CardList where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CardList where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

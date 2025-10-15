{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.Webhook where

import Data.Aeson
import qualified Data.Map as M
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Kernel.Prelude
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

-- TODO make not required fields as optional or remove
-- TODO check duplication with other Stripe types
data WebhookReq = WebhookReq
  { id :: Text,
    _object :: Text,
    api_version :: Text,
    created :: Integer,
    _data :: WebhookReqData,
    livemode :: Bool,
    pending_webhooks :: Integer,
    request :: WebhookRequest,
    _type :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON WebhookReq where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON WebhookReq where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema WebhookReq where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

newtype WebhookReqData = WebhookReqData
  { _object :: SetupIntent
  }
  deriving stock (Show, Generic)

instance FromJSON WebhookReqData where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON WebhookReqData where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema WebhookReqData where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data WebhookRequest = WebhookRequest
  { id :: Maybe Text,
    idempotency_key :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SetupIntent = SetupIntent
  { id :: Text,
    _object :: Text,
    application :: Maybe Text,
    automatic_payment_methods :: Maybe Value,
    cancellation_reason :: Maybe Text,
    client_secret :: Text,
    created :: Integer,
    customer :: Maybe Text,
    description :: Maybe Text,
    flow_directions :: Maybe [Text],
    last_setup_error :: Maybe Value,
    latest_attempt :: Maybe Text,
    livemode :: Bool,
    mandate :: Maybe Text,
    metadata :: M.Map Text Text,
    next_action :: Maybe Value,
    on_behalf_of :: Maybe Text,
    payment_method :: Text,
    payment_method_options :: PaymentMethodOptions,
    payment_method_types :: [Text],
    single_use_mandate :: Maybe Text,
    status :: Text,
    usage :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON SetupIntent where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON SetupIntent where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema SetupIntent where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

newtype PaymentMethodOptions = PaymentMethodOptions
  { acss_debit :: Maybe ACSSDebit
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ACSSDebit = ACSSDebit
  { currency :: Text,
    mandate_options :: MandateOptions,
    verification_method :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateOptions = MandateOptions
  { interval_description :: Text,
    payment_schedule :: Text,
    transaction_type :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

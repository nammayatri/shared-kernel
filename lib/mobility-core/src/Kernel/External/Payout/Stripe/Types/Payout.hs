{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.External.Payout.Stripe.Types.Payout where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Kernel.External.Payout.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Utils.JSON
import qualified Kernel.Utils.Schema as S
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- Payout Types
newtype PayoutId = PayoutId Text
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, FromHttpApiData, ToHttpApiData)

data PayoutStatus
  = PAYOUT_PENDING
  | PAYOUT_IN_TRANSIT
  | PAYOUT_PAID
  | PAYOUT_FAILED
  | PAYOUT_CANCELED
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON PayoutStatus where
  parseJSON = withText "PayoutStatus" $ \case
    "pending" -> pure PAYOUT_PENDING
    "in_transit" -> pure PAYOUT_IN_TRANSIT
    "paid" -> pure PAYOUT_PAID
    "failed" -> pure PAYOUT_FAILED
    "canceled" -> pure PAYOUT_CANCELED
    _ -> fail "Invalid payout status"

instance ToJSON PayoutStatus where
  toJSON PAYOUT_PENDING = String "pending"
  toJSON PAYOUT_IN_TRANSIT = String "in_transit"
  toJSON PAYOUT_PAID = String "paid"
  toJSON PAYOUT_FAILED = String "failed"
  toJSON PAYOUT_CANCELED = String "canceled"

instance ToHttpApiData PayoutStatus where
  toQueryParam PAYOUT_PENDING = "pending"
  toQueryParam PAYOUT_IN_TRANSIT = "in_transit"
  toQueryParam PAYOUT_PAID = "paid"
  toQueryParam PAYOUT_FAILED = "failed"
  toQueryParam PAYOUT_CANCELED = "canceled"

instance FromHttpApiData PayoutStatus where
  parseQueryParam "pending" = Right PAYOUT_PENDING
  parseQueryParam "in_transit" = Right PAYOUT_IN_TRANSIT
  parseQueryParam "paid" = Right PAYOUT_PAID
  parseQueryParam "failed" = Right PAYOUT_FAILED
  parseQueryParam "canceled" = Right PAYOUT_CANCELED
  parseQueryParam _ = Left "Invalid payout status"

data PayoutType = Card | BankAccount
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON PayoutType where
  parseJSON = withText "PayoutType" $ \case
    "card" -> pure Card
    "bank_account" -> pure BankAccount
    _ -> fail "Invalid payout type"

instance ToJSON PayoutType where
  toJSON Card = String "card"
  toJSON BankAccount = String "bank_account"

instance ToHttpApiData PayoutType where
  toQueryParam Card = "card"
  toQueryParam BankAccount = "bank_account"

data PayoutMethod = Instant | Standard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance ToJSON PayoutMethod where
  toJSON Instant = String "instant"
  toJSON Standard = String "standard"

instance FromJSON PayoutMethod where
  parseJSON = withText "PayoutMethod" $ \case
    "instant" -> pure Instant
    "standard" -> pure Standard
    _ -> fail "Invalid payout type"

instance ToHttpApiData PayoutMethod where
  toQueryParam Instant = "instant"
  toQueryParam Standard = "standard"

-- Create Payout Request
data CreatePayoutReq = CreatePayoutReq
  { amount :: Int, -- Amount in cents
    currency :: Text,
    description :: Maybe Text,
    destination :: Maybe Text, -- Bank account or card ID
    method :: Maybe PayoutMethod,
    sourceType :: Maybe PayoutType,
    statementDescriptor :: Maybe Text,
    metadata :: Maybe Metadata
  }
  deriving (Show, Generic)

instance ToForm CreatePayoutReq where
  toForm CreatePayoutReq {..} =
    Form $
      HM.fromList $
        [ ("amount", [toQueryParam amount]),
          ("currency", [toQueryParam currency])
        ]
          <> catMaybes
            [ ("description",) . pure <$> description,
              ("destination",) . pure <$> destination,
              ("method",) . pure . toQueryParam <$> method,
              ("source_type",) . pure . toQueryParam <$> sourceType,
              ("statement_descriptor",) . pure <$> statementDescriptor,
              ("metadata[order_id]",) . pure . toQueryParam <$> ((.order_id) =<< metadata)
            ]

-- Payout Object Response
data PayoutObject = PayoutObject
  { id :: PayoutId,
    amount :: Int,
    currency :: Text,
    status :: PayoutStatus,
    _type :: PayoutType,
    method :: PayoutMethod,
    description :: Maybe Text,
    destination :: Maybe Text,
    created :: UTCTime,
    arrival_date :: Maybe UTCTime,
    statement_descriptor :: Maybe Text,
    metadata :: Map.Map Text Text,
    failure_code :: Maybe Text,
    failure_message :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON PayoutObject where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PayoutObject where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema PayoutObject where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

-- List Payouts Response
data PayoutList = PayoutList
  { _data :: [PayoutObject],
    has_more :: Bool
  }
  deriving (Show, Generic)

instance FromJSON PayoutList where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PayoutList where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema PayoutList where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

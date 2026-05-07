{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payout.Stripe.Types.Payout where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
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

-- Payout status from driver/fleet connected account to driver/fleet card or bank account
data ExternalPayoutStatus
  = EXTERNAL_PAYOUT_PENDING
  | EXTERNAL_PAYOUT_IN_TRANSIT
  | EXTERNAL_PAYOUT_PAID
  | EXTERNAL_PAYOUT_FAILED
  | EXTERNAL_PAYOUT_CANCELED
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ExternalPayoutStatus where
  parseJSON = withText "ExternalPayoutStatus" $ \case
    "pending" -> pure EXTERNAL_PAYOUT_PENDING
    "in_transit" -> pure EXTERNAL_PAYOUT_IN_TRANSIT
    "paid" -> pure EXTERNAL_PAYOUT_PAID
    "failed" -> pure EXTERNAL_PAYOUT_FAILED
    "canceled" -> pure EXTERNAL_PAYOUT_CANCELED
    _ -> fail "Invalid payout status"

instance ToJSON ExternalPayoutStatus where
  toJSON EXTERNAL_PAYOUT_PENDING = String "pending"
  toJSON EXTERNAL_PAYOUT_IN_TRANSIT = String "in_transit"
  toJSON EXTERNAL_PAYOUT_PAID = String "paid"
  toJSON EXTERNAL_PAYOUT_FAILED = String "failed"
  toJSON EXTERNAL_PAYOUT_CANCELED = String "canceled"

instance ToHttpApiData ExternalPayoutStatus where
  toQueryParam EXTERNAL_PAYOUT_PENDING = "pending"
  toQueryParam EXTERNAL_PAYOUT_IN_TRANSIT = "in_transit"
  toQueryParam EXTERNAL_PAYOUT_PAID = "paid"
  toQueryParam EXTERNAL_PAYOUT_FAILED = "failed"
  toQueryParam EXTERNAL_PAYOUT_CANCELED = "canceled"

instance FromHttpApiData ExternalPayoutStatus where
  parseQueryParam "pending" = Right EXTERNAL_PAYOUT_PENDING
  parseQueryParam "in_transit" = Right EXTERNAL_PAYOUT_IN_TRANSIT
  parseQueryParam "paid" = Right EXTERNAL_PAYOUT_PAID
  parseQueryParam "failed" = Right EXTERNAL_PAYOUT_FAILED
  parseQueryParam "canceled" = Right EXTERNAL_PAYOUT_CANCELED
  parseQueryParam _ = Left "Invalid payout status"

$(mkBeamInstancesForEnum ''ExternalPayoutStatus)

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
    source_type :: Maybe PayoutType,
    statement_descriptor :: Maybe Text,
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
              ("source_type",) . pure . toQueryParam <$> source_type,
              ("statement_descriptor",) . pure <$> statement_descriptor,
              ("metadata[order_id]",) . pure . toQueryParam <$> (metadata >>= (.order_id)),
              ("metadata[customer_id]",) . pure . toQueryParam <$> (metadata >>= (.customer_id)),
              ("metadata[order_type]",) . pure . toQueryParam <$> (metadata >>= (.order_type))
            ]

-- Payout Object Response
data PayoutObject = PayoutObject
  { id :: PayoutId,
    amount :: Int,
    currency :: Text,
    status :: ExternalPayoutStatus,
    _type :: PayoutType,
    method :: PayoutMethod,
    description :: Maybe Text,
    destination :: Maybe Text,
    created :: POSIXTime,
    arrival_date :: Maybe POSIXTime,
    statement_descriptor :: Maybe Text,
    metadata :: Maybe Metadata,
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

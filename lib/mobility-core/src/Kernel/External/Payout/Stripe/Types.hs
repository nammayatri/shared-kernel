{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.External.Payout.Stripe.Types where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Kernel.Prelude
import Kernel.Utils.JSON
import qualified Kernel.Utils.Schema as S
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..))

-- Payout Types
newtype PayoutId = PayoutId Text
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, FromHttpApiData, ToHttpApiData)

data PayoutStatus
  = PayoutPending
  | PayoutInTransit
  | PayoutPaid
  | PayoutFailed
  | PayoutCanceled
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON PayoutStatus where
  parseJSON = withText "PayoutStatus" $ \case
    "pending" -> pure PayoutPending
    "in_transit" -> pure PayoutInTransit
    "paid" -> pure PayoutPaid
    "failed" -> pure PayoutFailed
    "canceled" -> pure PayoutCanceled
    _ -> fail "Invalid payout status"

instance ToJSON PayoutStatus where
  toJSON PayoutPending = String "pending"
  toJSON PayoutInTransit = String "in_transit"
  toJSON PayoutPaid = String "paid"
  toJSON PayoutFailed = String "failed"
  toJSON PayoutCanceled = String "canceled"

-- instance ToHttpApiData PayoutStatus where
--   toQueryParam PayoutPending = "pending"
--   toQueryParam PayoutInTransit = "in_transit"
--   toQueryParam PayoutPaid = "paid"
--   toQueryParam PayoutFailed = "failed"
--   toQueryParam PayoutCanceled = "canceled"

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
    metadata :: Maybe (Map.Map Text Text)
  }
  deriving (Show, Generic)

instance ToForm CreatePayoutReq where
  toForm CreatePayoutReq {..} =
    Form $
      HM.fromList $
        [ ("amount", [toQueryParam amount]),
          ("currency", [toQueryParam currency])
        ]
          ++ maybeToForm "description" (fmap pure description)
          ++ maybeToForm "destination" (fmap pure destination)
          ++ maybeToForm "method" (fmap (pure . toQueryParam) method)
          ++ maybeToForm "source_type" (fmap (pure . toQueryParam) sourceType)
          ++ maybeToForm "statement_descriptor" (fmap pure statementDescriptor)
          ++ metadataToForm metadata
    where
      maybeToForm _ Nothing = []
      maybeToForm key (Just val) = [(key, val)]

      metadataToForm Nothing = []
      metadataToForm (Just meta) =
        [("metadata[" <> k <> "]", [v]) | (k, v) <- Map.toList meta]

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

-- instance FromJSON PayoutObject where
--   parseJSON = withObject "PayoutObject" $ \o -> do
--     payoutId <- PayoutId <$> o .: "id"
--     payoutAmount <- o .: "amount"
--     payoutCurrency <- o .: "currency"
--     payoutStatus <- o .: "status"
--     payoutType <- o .: "type"
--     payoutMethod <- o .: "method"
--     payoutDescription <- o .:? "description"
--     payoutDestination <- o .:? "destination"
--     payoutCreated <- o .: "created"
--     payoutArrivalDate <- o .:? "arrival_date"
--     payoutStatementDescriptor <- o .:? "statement_descriptor"
--     payoutMetadata <- o .:? "metadata" .!= mempty
--     payoutFailureCode <- o .:? "failure_code"
--     payoutFailureMessage <- o .:? "failure_message"
--     return PayoutObject {..}

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

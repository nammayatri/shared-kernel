{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.External.Payment.Stripe.Types.Refund where

import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Utils.JSON
import qualified Kernel.Utils.Schema as S
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..))

-- Only one of `charge` and `payment_intent` is mandatory
data RefundReq = RefundReq
  { charge :: Maybe Text, -- The identifier of the charge to refund
    payment_intent :: Maybe Text, -- The identifier of the PaymentIntent to refund
    amount :: Maybe Int, -- Amount to refund (in smallest currency unit)
    metadata :: Metadata, -- Set of key-value pairs for metadata
    reason :: Maybe RefundReason, -- Reason for the refund
    refund_application_fee :: Maybe Bool, -- Whether to refund the application fee
    reverse_transfer :: Maybe Bool, -- Whether to reverse the transfer
    instructions_email :: Maybe Text -- Email where instructions to submit bank account info will be sent
  }
  deriving (Show, Generic)

instance ToForm RefundReq where
  toForm RefundReq {..} =
    Form $
      HM.fromList $
        catMaybes
          [ ("charge",) . pure . toQueryParam <$> charge,
            ("payment_intent",) . pure . toQueryParam <$> payment_intent,
            ("amount",) . pure . toQueryParam <$> amount,
            ("metadata[order_short_id]",) . pure . toQueryParam <$> metadata.order_short_id,
            ("metadata[order_id]",) . pure . toQueryParam <$> metadata.order_id,
            ("metadata[refunds_id]",) . pure . toQueryParam <$> metadata.refunds_id,
            ("reason",) . pure . toQueryParam <$> reason,
            ("refund_application_fee",) . pure . toQueryParam <$> refund_application_fee,
            ("reverse_transfer",) . pure . toQueryParam <$> reverse_transfer,
            ("instructions_email",) . pure . toQueryParam <$> instructions_email
          ]

newtype RefundId = RefundId {getRefundId :: Text}
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, FromHttpApiData, ToHttpApiData)

-- | The reference is nested under the destination payment method, so the key is dynamic:
--   destination_details.card.reference, .eu_bank_transfer.reference, etc. The sibling `type` names it.
data RefundDestinationDetails = RefundDestinationDetails
  { _type :: Text, -- Names the nested key holding the reference; not the nested object's own 'type'
    reference :: Maybe Text, -- The number the customer can trace with their bank
    reference_status :: Maybe Text, -- pending | available | unavailable
    reference_type :: Maybe Text -- e.g. acquirer_reference_number; cards only
  }
  deriving stock (Show, Generic)

instance FromJSON RefundDestinationDetails where
  parseJSON = withObject "RefundDestinationDetails" $ \obj -> do
    _type <- obj .: "type"
    obj .:? AK.fromText _type >>= \case
      Nothing -> pure RefundDestinationDetails {reference = Nothing, reference_status = Nothing, reference_type = Nothing, ..}
      Just details -> do
        reference <- details .:? "reference"
        reference_status <- details .:? "reference_status"
        reference_type <- details .:? "reference_type"
        pure RefundDestinationDetails {..}

instance ToJSON RefundDestinationDetails where
  toJSON RefundDestinationDetails {..} =
    object
      [ "type" .= _type,
        AK.fromText _type
          .= object
            ( catMaybes
                [ ("reference" .=) <$> reference,
                  ("reference_status" .=) <$> reference_status,
                  ("reference_type" .=) <$> reference_type
                ]
            )
      ]

instance ToSchema RefundDestinationDetails where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data RefundObject = RefundObject
  { id :: RefundId,
    _object :: Text, -- Value is 'refund'
    amount :: Int, -- Amount refunded in smallest currency unit
    balance_transaction :: Maybe Text, -- Balance transaction that describes the impact of this refund on your account balance
    charge :: Maybe Text, -- ID of the charge that was refunded
    created :: Int, -- Time at which the refund was created
    currency :: Text, -- Currency of the refund
    destination_details :: Maybe RefundDestinationDetails, -- Where the refund was sent, and its bank-traceable reference
    metadata :: Maybe Metadata, -- Set of key-value pairs attached to the refund
    payment_intent :: Maybe Text, -- ID of the PaymentIntent that was refunded
    reason :: Maybe RefundReason, -- Reason for the refund
    receipt_number :: Maybe Text, -- Receipt number for the refund
    source_transfer_reversal :: Maybe Text, -- Transfer reversal that is associated with the refund
    status :: RefundStatus, -- Status of the refund
    failure_balance_transaction :: Maybe Text, -- Balance transaction ID when refund fails
    failure_reason :: Maybe Text, -- Reason code for the refund failure
    transfer_reversal :: Maybe Text -- ID of the transfer reversal
  }
  deriving stock (Show, Generic)

instance HideSecrets RefundObject where
  hideSecrets = identity

instance FromJSON RefundObject where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON RefundObject where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema RefundObject where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data RefundStatus
  = REFUND_SUCCEEDED
  | REFUND_PENDING
  | REFUND_FAILED
  | REFUND_CANCELED
  | REFUND_REQUIRES_ACTION
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

refundStatusJsonOptions :: Options
refundStatusJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "REFUND_SUCCEEDED" -> "succeeded"
        "REFUND_PENDING" -> "pending"
        "REFUND_FAILED" -> "failed"
        "REFUND_CANCELED" -> "canceled"
        "REFUND_REQUIRES_ACTION" -> "requires_action"
        x -> x
    }

instance FromJSON RefundStatus where
  parseJSON = genericParseJSON refundStatusJsonOptions

instance ToJSON RefundStatus where
  toJSON = genericToJSON refundStatusJsonOptions

data RefundReason
  = DUPLICATE
  | FRAUDULENT
  | REQUESTED_BY_CUSTOMER
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

refundReasonJsonOptions :: Options
refundReasonJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "DUPLICATE" -> "duplicate"
        "FRAUDULENT" -> "fraudulent"
        "REQUESTED_BY_CUSTOMER" -> "requested_by_customer"
        x -> x
    }

instance FromJSON RefundReason where
  parseJSON = genericParseJSON refundReasonJsonOptions

instance ToJSON RefundReason where
  toJSON = genericToJSON refundReasonJsonOptions

instance ToHttpApiData RefundReason where
  toQueryParam DUPLICATE = "duplicate"
  toQueryParam FRAUDULENT = "fraudulent"
  toQueryParam REQUESTED_BY_CUSTOMER = "requested_by_customer"

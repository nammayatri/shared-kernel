{-# LANGUAGE DerivingStrategies #-}

-- | Stripe webhook payloads for the dedicated Connect payout webhook endpoint
-- (separate from payment webhooks; separate Stripe config).
--
-- Includes @payout.*@ events and @transfer.*@ for a future transfer webhook or
-- the same endpoint if both event classes are subscribed.
module Kernel.External.Payout.Stripe.Types.Webhook where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Bimap as BM
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.External.Payment.Stripe.Types.Common (Event)
import Kernel.External.Payout.Stripe.Types.Payout (PayoutObject)
import Kernel.External.Payout.Stripe.Types.Transfer (TransferObject)
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

-- | Stripe @payout.*@ and @transfer.*@ webhook @type@ strings (Connect payout flow).
data PayoutStripeWebhookEventType
  = -- Payout (data.object is a payout)
    PayoutCanceled
  | PayoutCreated
  | PayoutFailed
  | PayoutPaid
  | PayoutReconciliationCompleted
  | PayoutUpdated
  | -- Transfer (data.object is a transfer); for future transfer webhook
    TransferCreated
  | TransferReversed
  | TransferUpdated
  | -- Unknown
    PayoutStripeWebhookCustomEvent Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToSchema)

payoutStripeWebhookEventTypeBimap :: BM.Bimap PayoutStripeWebhookEventType Text
payoutStripeWebhookEventTypeBimap =
  BM.fromList
    [ (PayoutCanceled, "payout.canceled"),
      (PayoutCreated, "payout.created"),
      (PayoutFailed, "payout.failed"),
      (PayoutPaid, "payout.paid"),
      (PayoutReconciliationCompleted, "payout.reconciliation_completed"),
      (PayoutUpdated, "payout.updated"),
      (TransferCreated, "transfer.created"),
      (TransferReversed, "transfer.reversed"),
      (TransferUpdated, "transfer.updated")
    ]

instance FromJSON PayoutStripeWebhookEventType where
  parseJSON = withText "PayoutStripeWebhookEventType" $ \txt ->
    pure $ fromMaybe (PayoutStripeWebhookCustomEvent txt) $ BM.lookupR txt payoutStripeWebhookEventTypeBimap

instance ToJSON PayoutStripeWebhookEventType where
  toJSON = String . payoutStripeWebhookEventTypeToText

payoutStripeWebhookEventTypeToText :: PayoutStripeWebhookEventType -> Text
payoutStripeWebhookEventTypeToText eventType = case BM.lookup eventType payoutStripeWebhookEventTypeBimap of
  Just txt -> txt
  Nothing -> case eventType of
    PayoutStripeWebhookCustomEvent t -> t
    _ -> show eventType

data PayoutStripeWebhookReq = PayoutStripeWebhookReq
  { id :: Id Event,
    _object :: Text,
    api_version :: Text,
    created :: POSIXTime,
    _data :: PayoutStripeWebhookReqData,
    livemode :: Bool,
    pending_webhooks :: Integer,
    request :: PayoutStripeWebhookRequest,
    _type :: PayoutStripeWebhookEventType
  }
  deriving stock (Show, Generic)

instance HideSecrets PayoutStripeWebhookReq where
  hideSecrets PayoutStripeWebhookReq {..} =
    PayoutStripeWebhookReq
      { _data = hideSecrets @PayoutStripeWebhookReqData _data,
        ..
      }

instance FromJSON PayoutStripeWebhookReq where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON PayoutStripeWebhookReq where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema PayoutStripeWebhookReq where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

newtype PayoutStripeWebhookReqData = PayoutStripeWebhookReqData
  { _object :: PayoutStripeWebhookObject
  }
  deriving stock (Show, Generic)

instance HideSecrets PayoutStripeWebhookReqData where
  hideSecrets PayoutStripeWebhookReqData {..} =
    PayoutStripeWebhookReqData
      { _object = hideSecrets @PayoutStripeWebhookObject _object
      }

instance FromJSON PayoutStripeWebhookReqData where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON PayoutStripeWebhookReqData where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema PayoutStripeWebhookReqData where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data PayoutStripeWebhookRequest = PayoutStripeWebhookRequest
  { id :: Maybe Text,
    idempotency_key :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PayoutStripeWebhookObject
  = ObjectPayout PayoutObject
  | ObjectTransfer TransferObject
  | PayoutStripeWebhookCustomObject Text Value
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

getPayoutStripeWebhookObjectType :: PayoutStripeWebhookObject -> Text
getPayoutStripeWebhookObjectType = \case
  ObjectPayout _ -> "payout"
  ObjectTransfer obj -> obj._object
  PayoutStripeWebhookCustomObject objType _val -> objType

instance HideSecrets PayoutStripeWebhookObject where
  hideSecrets = \case
    ObjectPayout a -> ObjectPayout a
    ObjectTransfer a -> ObjectTransfer a
    PayoutStripeWebhookCustomObject objType _val -> PayoutStripeWebhookCustomObject objType A.Null

instance ToJSON PayoutStripeWebhookObject where
  toJSON = \case
    ObjectPayout a -> toJSON @PayoutObject a
    ObjectTransfer a -> toJSON @TransferObject a
    PayoutStripeWebhookCustomObject _objType val -> val

instance FromJSON PayoutStripeWebhookObject where
  parseJSON val = flip (withObject "PayoutStripeWebhookObject") val $ \obj -> do
    objectType :: Text <- obj .: "object"
    case objectType of
      "payout" -> ObjectPayout <$> parseJSON @PayoutObject val
      "transfer" -> ObjectTransfer <$> parseJSON @TransferObject val
      unknown -> pure $ PayoutStripeWebhookCustomObject unknown val

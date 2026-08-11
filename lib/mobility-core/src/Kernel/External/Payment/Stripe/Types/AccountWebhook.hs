{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.AccountWebhook where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Bimap as BM
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.External.Payment.Stripe.Types.Accounts (Requirements)
import Kernel.External.Payment.Stripe.Types.Common (AccountId, Event)
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

data AccountStripeWebhookEventType
  = AccountUpdated
  | AccountStripeWebhookCustomEvent Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToSchema)

accountStripeWebhookEventTypeBimap :: BM.Bimap AccountStripeWebhookEventType Text
accountStripeWebhookEventTypeBimap =
  BM.fromList
    [(AccountUpdated, "account.updated")]

instance FromJSON AccountStripeWebhookEventType where
  parseJSON = withText "AccountStripeWebhookEventType" $ \txt ->
    pure $ fromMaybe (AccountStripeWebhookCustomEvent txt) $ BM.lookupR txt accountStripeWebhookEventTypeBimap

instance ToJSON AccountStripeWebhookEventType where
  toJSON = String . accountStripeWebhookEventTypeToText

accountStripeWebhookEventTypeToText :: AccountStripeWebhookEventType -> Text
accountStripeWebhookEventTypeToText eventType = case BM.lookup eventType accountStripeWebhookEventTypeBimap of
  Just txt -> txt
  Nothing -> case eventType of
    AccountStripeWebhookCustomEvent t -> t
    _ -> show eventType

data AccountObject = AccountObject
  { id :: AccountId,
    _object :: Text,
    charges_enabled :: Bool,
    payouts_enabled :: Bool,
    details_submitted :: Bool,
    requirements :: Maybe Requirements,
    future_requirements :: Maybe Requirements
  }
  deriving stock (Show, Generic)

instance FromJSON AccountObject where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON AccountObject where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema AccountObject where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

instance HideSecrets AccountObject where
  hideSecrets = identity

data AccountStripeWebhookObject
  = ObjectAccount AccountObject
  | AccountStripeWebhookCustomObject Text Value
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

getAccountStripeWebhookObjectType :: AccountStripeWebhookObject -> Text
getAccountStripeWebhookObjectType = \case
  ObjectAccount obj -> obj._object
  AccountStripeWebhookCustomObject objType _val -> objType

instance HideSecrets AccountStripeWebhookObject where
  hideSecrets = \case
    ObjectAccount a -> ObjectAccount a
    AccountStripeWebhookCustomObject objType _val -> AccountStripeWebhookCustomObject objType A.Null

instance ToJSON AccountStripeWebhookObject where
  toJSON = \case
    ObjectAccount a -> toJSON @AccountObject a
    AccountStripeWebhookCustomObject _objType val -> val

instance FromJSON AccountStripeWebhookObject where
  parseJSON val = flip (withObject "AccountStripeWebhookObject") val $ \obj -> do
    objectType :: Text <- obj .: "object"
    case objectType of
      "account" -> ObjectAccount <$> parseJSON @AccountObject val
      unknown -> pure $ AccountStripeWebhookCustomObject unknown val

newtype AccountStripeWebhookReqData = AccountStripeWebhookReqData
  { _object :: AccountStripeWebhookObject
  }
  deriving stock (Show, Generic)

instance HideSecrets AccountStripeWebhookReqData where
  hideSecrets AccountStripeWebhookReqData {..} =
    AccountStripeWebhookReqData
      { _object = hideSecrets @AccountStripeWebhookObject _object
      }

instance FromJSON AccountStripeWebhookReqData where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON AccountStripeWebhookReqData where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema AccountStripeWebhookReqData where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data AccountStripeWebhookRequest = AccountStripeWebhookRequest
  { id :: Maybe Text,
    idempotency_key :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AccountStripeWebhookReq = AccountStripeWebhookReq
  { id :: Id Event,
    _object :: Text,
    api_version :: Text,
    created :: POSIXTime,
    _data :: AccountStripeWebhookReqData,
    livemode :: Bool,
    pending_webhooks :: Integer,
    request :: AccountStripeWebhookRequest,
    _type :: AccountStripeWebhookEventType
  }
  deriving stock (Show, Generic)

instance HideSecrets AccountStripeWebhookReq where
  hideSecrets AccountStripeWebhookReq {..} =
    AccountStripeWebhookReq
      { _data = hideSecrets @AccountStripeWebhookReqData _data,
        ..
      }

instance FromJSON AccountStripeWebhookReq where
  parseJSON = genericParseJSON J.stripPrefixUnderscoreIfAny

instance ToJSON AccountStripeWebhookReq where
  toJSON = genericToJSON J.stripPrefixUnderscoreIfAny

instance ToSchema AccountStripeWebhookReq where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

module Kernel.External.VoiceSdk.BreezeBuddy.Types where

import qualified Data.Aeson as A
import Kernel.External.Encryption
import Kernel.Prelude

data BreezeBuddySdkConfig = BreezeBuddySdkConfig
  { url :: BaseUrl,
    -- | Bearer token used to authenticate against the BreezeBuddy voice agent APIs.
    apiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Request body for @POST /agent/voice/breeze-buddy/leads@.
-- Optional fields are omitted from the JSON when 'Nothing'.
data BreezeBuddyLeadRequest = BreezeBuddyLeadRequest
  { request_id :: Text,
    template :: Text,
    -- | Template specific payload. @customer_mobile_number@ is mandatory and must be E.164 (e.g. "+14155551234").
    payload :: A.Value,
    reseller_id :: Text,
    merchant_id :: Maybe Text,
    reporting_webhook_url :: Maybe Text,
    execution_mode :: Maybe Text,
    is_playground :: Maybe Bool,
    configurations_override :: Maybe A.Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON BreezeBuddyLeadRequest where
  parseJSON = A.genericParseJSON A.defaultOptions {A.omitNothingFields = True}

instance ToJSON BreezeBuddyLeadRequest where
  toJSON = A.genericToJSON A.defaultOptions {A.omitNothingFields = True}

-- | Success (201) response for @POST .../leads@.
data BreezeBuddyLeadResponse = BreezeBuddyLeadResponse
  { -- | Always "queued" on success.
    status :: Text,
    -- | Canonical lead identifier; pass this as @lead_id@ to the connect endpoint.
    lead_call_tracker_id :: Text,
    -- | Echoes the submitted @request_id@.
    order_id :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype BreezeBuddyConnectRequest = BreezeBuddyConnectRequest
  { lead_id :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Success response for @POST .../connect@.
data BreezeBuddyConnectResponse = BreezeBuddyConnectResponse
  { room_url :: Text,
    token :: Text,
    session_id :: Text,
    lead_id :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

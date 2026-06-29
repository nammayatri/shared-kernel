module Kernel.External.VoiceSdk.Interface.Types where

import qualified Data.Aeson as A
import qualified Kernel.External.VoiceSdk.BreezeBuddy.Types as BreezeBuddy
import Kernel.Prelude

data CreateLeadReq = CreateLeadReq
  { requestId :: Text,
    template :: Text,
    payload :: A.Value,
    resellerId :: Text,
    merchantId :: Maybe Text,
    reportingWebhookUrl :: Maybe Text,
    executionMode :: Maybe Text,
    isPlayground :: Maybe Bool,
    configurationsOverride :: Maybe A.Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateLeadResp = CreateLeadResp
  { status :: Text,
    -- | Canonical lead identifier; feed this into 'ConnectReq.leadId'.
    leadCallTrackerId :: Text,
    orderId :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ConnectReq = ConnectReq
  { leadId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConnectResp = ConnectResp
  { roomUrl :: Text,
    token :: Text,
    sessionId :: Text,
    leadId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data VoiceSdkConfig
  = BreezeBuddyVoiceSdkConfig BreezeBuddy.BreezeBuddySdkConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

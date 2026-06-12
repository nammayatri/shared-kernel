{-# LANGUAGE TemplateHaskell #-}

module Kernel.Webhook.Types where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.External.Encryption
import Kernel.Prelude

data ExternalWebhookConfigs = ExternalWebhookConfigs
  { baseUrl :: BaseUrl,
    apiKey :: EncryptedField 'AsEncrypted Text,
    merchantId :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WebhookEvent
  = MANDATE
  | SERVICE_STARTED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WebhookStatus = DELIVERED | FAILED | PENDING | RETRIES_ENDED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WebhookDeliveryType = BATCHING | REAL_TIME deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data ExternalWebhookData = ExternalWebhookData
  { id :: Text,
    shortId :: Text,
    eventName :: WebhookEvent,
    webhookData :: Value
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''WebhookEvent)
$(mkBeamInstancesForEnumAndList ''WebhookDeliveryType)
$(mkBeamInstancesForEnumAndList ''WebhookStatus)

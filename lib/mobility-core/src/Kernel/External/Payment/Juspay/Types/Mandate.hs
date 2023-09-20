{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.External.Payment.Juspay.Types.Mandate where

import Kernel.External.Payment.Juspay.Types.Common (NotificationStatus, TransactionStatus)
import Kernel.Prelude
import Servant (ToHttpApiData (..))
import Web.FormUrlEncoded

--- For Mandate Notifications ---
data MandateNotificationReq = MandateNotificationReq
  { command :: Text,
    object_reference_id :: Text,
    source_info :: SourceInfo,
    description :: Text
  }
  deriving (Eq, Show, Generic)

data MandateNotificationRes = MandateNotificationRes
  { id :: Text,
    source_info :: SourceInfo,
    object_reference_id :: Text,
    provider_name :: Text,
    notification_type :: Text,
    description :: Text,
    status :: NotificationStatus,
    date_created :: Text,
    last_updated :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data SourceInfo = SourceInfo
  { amount :: Text,
    txn_date :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ToForm MandateNotificationReq where
  toForm MandateNotificationReq {..} =
    [ ("command", toQueryParam command),
      ("object_reference_id", toQueryParam object_reference_id),
      ("source_info", toQueryParam ("{\"amount\" : \"" <> source_info.amount <> "\"," <> " \"txn_date\" : \"" <> source_info.txn_date <> "\"}")),
      ("description", toQueryParam description)
    ]

---- Notification status response ------

data NotificationStatusResp = NotificationStatusResp
  { id :: Text,
    source_object :: Text,
    source_object_id :: Text, -- mandate Id in this case --
    object_reference_id :: Text,
    provider_name :: Text,
    notification_type :: Text,
    source_info :: SourceInfo,
    provider_response :: ProviderResponse,
    description :: Text,
    status :: NotificationStatus,
    date_created :: Text,
    last_updated :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ProviderResponse = ProviderResponse
  { provider_ref_id :: Text,
    notification_date :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

--- For Mandate Execution ---
data MandateOrder = MandateOrder
  { orderId :: Text,
    orderAmount :: Text,
    orderCustomerId :: Text
  }

data MandateInfo = MandateInfo
  { notificationId :: Text,
    executionDate :: Text
  }

data MandateExecutionReq = MandateExecutionReq
  { order :: MandateOrder,
    merchantId :: Text,
    mandateId :: Text,
    mandate :: MandateInfo,
    format :: Text
  }

instance ToForm MandateExecutionReq where
  toForm MandateExecutionReq {..} =
    [ ("order.order_id", toQueryParam (orderId order)),
      ("order.amount", toQueryParam (orderAmount order)),
      ("order.customer_id", toQueryParam (orderCustomerId order)),
      ("mandate_id", toQueryParam mandateId),
      ("mandate.notification_id", toQueryParam (notificationId mandate)),
      ("mandate.execution_date", toQueryParam (executionDate mandate)),
      ("merchant_id", toQueryParam merchantId),
      ("format", toQueryParam format)
    ]

data MandateExecutionRes = MandateExecutionRes
  { order_id :: Text,
    txn_id :: Text,
    txn_uuid :: Text,
    status :: TransactionStatus
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

--- For Mandate Revoke --
newtype MandateRevokeReq = MandateRevokeReq
  { command :: Text
  }
  deriving (Eq, Show, Generic, ToForm)

data MandateRevokeRes = MandateRevokeRes
  { mandate_id :: Text,
    mandate_status :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

--- For Mandate Pause ---

data MandatePauseReq = MandatePauseReq
  { command :: Text,
    pause_start_date :: Text,
    pause_end_date :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, ToForm)

--- For Mandate Resume ---

data MandateResumeReq = MandateResumeReq
  { command :: Text,
    resume_date :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, ToForm)

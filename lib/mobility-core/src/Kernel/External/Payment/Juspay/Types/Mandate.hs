{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.External.Payment.Juspay.Types.Mandate where

import Kernel.Prelude
import Servant (ToHttpApiData (..))
import Web.FormUrlEncoded

--- For Mandate Notifications ---
data MandateNotificationReq = MandateNotificationReq
  { command :: Text,
    objectRefId :: Text,
    sourceInfo :: SourceInfo,
    description :: Text
  }
  deriving (Eq, Show, Generic)

data MandateNotificationRes = MandateNotificationRes
  { id :: Text,
    source_object :: Text,
    object_reference_id :: Text,
    provider_name :: Text,
    notification_type :: Text,
    description :: Text,
    status :: Text,
    date_created :: Text,
    last_updated :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data SourceInfo = SourceInfo
  { sourceAmount :: Text,
    txnDate :: Text
  }
  deriving (Eq, Show, Generic)

instance ToForm MandateNotificationReq where
  toForm MandateNotificationReq {..} =
    [ ("command", toQueryParam command),
      ("object_reference_id", toQueryParam objectRefId),
      ("source_info.amount", toQueryParam (sourceAmount sourceInfo)),
      ("source_info.txn_date", toQueryParam (txnDate sourceInfo)),
      ("description", toQueryParam description)
    ]

--- For Mandate Execution ---
data MandateOrder = MandateOrder
  { orderId :: Text,
    orderAmount :: Text,
    orderCustomerId :: Text
  }

data MandateInfo = MandateInfo
  { notificationId :: Text,
    executionDate :: UTCTime
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
    status :: Text
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

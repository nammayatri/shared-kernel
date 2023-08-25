{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.External.Payment.Juspay.Types.Mandate where

import Kernel.Prelude
import Servant (ToHttpApiData (..))
import Web.FormUrlEncoded

-- import Kernel.Types.Common
-- import Kernel.Utils.JSON

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
    status :: Text,
    date_created :: Text,
    last_updated :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data SourceInfo = SourceInfo
  { source_amount :: Text,
    txn_date :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ToForm MandateNotificationReq where
  toForm MandateNotificationReq {..} =
    [ ("command", toQueryParam command),
      ("object_reference_id", toQueryParam object_reference_id),
      ("source_info.amount", toQueryParam (source_info.source_amount)),
      ("source_info.txn_date", toQueryParam (source_info.txn_date)),
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

-- --- For Mandate List ---
-- data MandateListResp = MandateListResp
--   {
--     list :: [MandateList],
--     total :: Int
--   }
--   deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
-- data MandateList = MandateList
--   {
--     paymentInfo :: PaymentInfo,
--     status :: MandateStatus,
--     startDate :: UTCTime,
--     endDate :: UTCTime,
--     maxAmount :: Money,
--     mandateToken :: Text,
--     mandateId :: Text,
--     currency :: Text
--   }
--   deriving (Eq, Show, Generic, ToSchema)

-- instance FromJSON MandateList where
--   parseJSON = genericParseJSON constructorsWithSnakeCase

-- instance ToJSON MandateList where
--   toJSON = genericToJSON constructorsWithSnakeCase
-- data PaymentInfo = PaymentInfo
--   {
--     upiDetails :: UPIDetails,
--     paymentMethodType :: Text,
--     paymentMethod :: Text
--   }
--   deriving (Eq, Show, Generic, ToSchema)

-- instance FromJSON PaymentInfo where
--   parseJSON = genericParseJSON constructorsWithSnakeCase
-- instance ToJSON PaymentInfo where
--   toJSON = genericToJSON constructorsWithSnakeCase
-- newtype UPIDetails = UPIDetails
--   {
--     payerVpa :: Text
--   }
--   deriving (Eq, Show, Generic, ToSchema)
-- instance FromJSON UPIDetails where
--   parseJSON = genericParseJSON constructorsWithSnakeCase
-- instance ToJSON UPIDetails where
--   toJSON = genericToJSON constructorsWithSnakeCase

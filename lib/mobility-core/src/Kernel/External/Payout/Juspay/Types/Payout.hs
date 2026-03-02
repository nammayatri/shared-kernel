{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payout.Juspay.Types.Payout where

import Data.Aeson as A
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude

--------------------------------------------------------------------------
-- Payout Create Order Types

data PayoutOrderStatus
  = READY_FOR_FULFILLMENT
  | FULFILLMENTS_SCHEDULED
  | FULFILLMENTS_FAILURE
  | FULFILLMENTS_SUCCESSFUL
  | FULFILLMENTS_CANCELLED
  | FULFILLMENTS_MANUAL_REVIEW
  | FULFILLED_PARTIALLY
  | INITIATED
  | FAILURE
  | SUCCESS
  | DISCARDED
  | MANUAL_REVIEW
  | CANCELLED
  | GATEWAY_SWITCHED
  | ERROR
  | INVALID
  | VALID
  | CONFLICTED
  | REVERSED
  deriving (Show, Generic, Ord, Read, FromJSON, ToJSON, ToSchema, Eq)

$(mkBeamInstancesForEnum ''PayoutOrderStatus)

data WebhookDetails = WebhookDetails
  { username :: Maybe Text,
    password :: Maybe Text,
    url :: Maybe Text,
    customHeader :: Maybe Text
  }
  deriving (Show, Generic, ToSchema, Eq)
  deriving anyclass (FromJSON, ToJSON)

data AdditionalInfo = AdditionalInfo
  { webhookDetails :: Maybe WebhookDetails,
    remark :: Maybe Text,
    isRetriable :: Maybe Bool,
    attemptThreshold :: Maybe Int
  }
  deriving (Show, Generic, ToSchema)
  deriving anyclass (FromJSON, ToJSON)

data AccountDetailsType = UPI_ID | ACCOUNT_IFSC
  deriving (Show, Generic, Read, Ord, Eq, ToSchema)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''AccountDetailsType)

data AccountDetails = AccountDetails
  { name :: Text,
    vpa :: Maybe Text,
    mobileNo :: Maybe Text,
    account :: Maybe Text,
    ifsc :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)
  deriving anyclass (FromJSON, ToJSON)

data BeneficiaryDetails = BeneficiaryDetails
  { details :: Maybe AccountDetails,
    detailsType :: Maybe AccountDetailsType
  }
  deriving (Show, Generic, ToSchema)

jsonOptions_ :: Options
jsonOptions_ =
  defaultOptions
    { fieldLabelModifier = \case
        "detailsType" -> "type"
        other -> other
    }

instance FromJSON BeneficiaryDetails where
  parseJSON = genericParseJSON jsonOptions_

instance ToJSON BeneficiaryDetails where
  toJSON = genericToJSON jsonOptions_

data TxnResponse = TxnResponse
  { status :: Text,
    requestId :: Maybe Text,
    amount :: Maybe Double
  }
  deriving (Show, Generic, ToSchema)
  deriving anyclass (FromJSON, ToJSON)

data Transaction = Transaction
  { txnResponse :: Maybe TxnResponse,
    updatedAt :: Text,
    transactionRef :: Text,
    status :: Text,
    responseMessage :: Maybe Text,
    responseCode :: Maybe Text,
    gatewayRefId :: Maybe Text,
    fulfillmentMethod :: Maybe Text,
    epgTxnId :: Maybe Text,
    createdAt :: Text,
    amount :: Double
  }
  deriving (Show, Generic, ToSchema)
  deriving anyclass (FromJSON, ToJSON)

data Fulfillment = Fulfillment
  { id :: Text,
    udf2 :: Maybe Text,
    udf1 :: Maybe Text,
    status :: Text,
    statusUpdatedAt :: Maybe Text,
    preferredMethodList :: Maybe [Text],
    merchantCustomerId :: Maybe Text,
    fulfillmentMethodList :: Maybe [Text],
    beneficiaryDetails :: Maybe BeneficiaryDetails,
    amount :: Double,
    additionalInfo :: Maybe AdditionalInfo,
    transactions :: Maybe [Transaction],
    createdAt :: Maybe Text,
    updatedAt :: Text
  }
  deriving (Show, Generic, ToSchema)
  deriving anyclass (FromJSON, ToJSON)

data PayoutOrderResp = PayoutOrderResp
  { orderId :: Text,
    status :: PayoutOrderStatus,
    orderType :: Maybe Text,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    amount :: Double,
    refunds :: Maybe [Text], -- will always be Nothing in CreatePayoutOrderResp
    payments :: Maybe [Text],
    fulfillments :: Maybe [Fulfillment],
    customerId :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON PayoutOrderResp where
  parseJSON = withObject "PayoutOrderResp" $ \v ->
    PayoutOrderResp
      <$> v .: "orderId"
      <*> v .: "status"
      <*> v .:? "type"
      <*> v .:? "udf1"
      <*> v .:? "udf2"
      <*> v .:? "udf3"
      <*> v .:? "udf4"
      <*> v .:? "udf5"
      <*> v .: "amount"
      <*> v .:? "refunds"
      <*> v .:? "payments"
      <*> v .:? "fulfillments"
      <*> v .:? "customerId"

instance ToJSON PayoutOrderResp where
  toJSON req =
    object $
      catMaybes
        [ Just $ "orderId" .= req.orderId,
          Just $ "status" .= req.status,
          Just $ "type" .= req.orderType,
          ("udf1" .=) <$> req.udf1,
          ("udf2" .=) <$> req.udf2,
          ("udf3" .=) <$> req.udf3,
          ("udf4" .=) <$> req.udf4,
          ("udf5" .=) <$> req.udf5,
          Just $ "amount" .= req.amount,
          ("refunds" .=) <$> req.refunds,
          ("payments" .=) <$> req.fulfillments,
          ("fulfillments" .=) <$> req.fulfillments,
          Just $ "customerId" .= req.customerId
        ]

data PayoutOrderReqFulfillment = PayoutOrderReqFulfillment
  { udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    preferredMethodList :: Maybe [Text],
    amount :: Double,
    additionalInfo :: Maybe AdditionalInfo,
    beneficiaryDetails :: Maybe BeneficiaryDetails
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreatePayoutOrderReq = CreatePayoutOrderReq
  { orderId :: Text,
    orderType :: Text, -- FULFILL_ONLY for payout order creation
    customerId :: Text,
    amount :: Double,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    customerPhone :: Text,
    customerEmail :: Text,
    fulfillments :: Maybe [PayoutOrderReqFulfillment]
  }
  deriving stock (Show, Generic)

instance ToJSON CreatePayoutOrderReq where
  toJSON req =
    object $
      catMaybes
        [ Just $ "orderId" .= req.orderId,
          Just $ "type" .= req.orderType,
          Just $ "customerId" .= req.customerId,
          Just $ "amount" .= req.amount,
          ("udf1" .=) <$> req.udf1,
          ("udf2" .=) <$> req.udf2,
          ("udf3" .=) <$> req.udf3,
          ("udf4" .=) <$> req.udf4,
          ("udf5" .=) <$> req.udf5,
          Just $ "customerPhone" .= req.customerPhone,
          Just $ "customerEmail" .= req.customerEmail,
          ("fulfillments" .=) <$> req.fulfillments
        ]

instance FromJSON CreatePayoutOrderReq where
  parseJSON = withObject "CreatePayoutOrderReq" $ \v ->
    CreatePayoutOrderReq
      <$> v .: "orderId"
      <*> v .: "type"
      <*> v .: "customerId"
      <*> v .: "amount"
      <*> v .:? "udf1"
      <*> v .:? "udf2"
      <*> v .:? "udf3"
      <*> v .:? "udf4"
      <*> v .:? "udf5"
      <*> v .: "customerPhone"
      <*> v .: "customerEmail"
      <*> v .:? "fulfillments"

---------------------------------------------------------------------------
-- Payout Order Status Types

type PayoutOrderStatusResp = PayoutOrderResp

type CreatePayoutOrderResp = PayoutOrderResp

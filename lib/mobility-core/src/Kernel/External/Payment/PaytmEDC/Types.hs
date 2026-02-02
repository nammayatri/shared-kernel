{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Kernel.External.Payment.PaytmEDC.Types
  ( module Kernel.External.Payment.PaytmEDC.Types,
  )
where

import Data.Aeson
import Kernel.Prelude

-- Req head for both sale and status enquiry
data PaytmEDCRequestHead = PaytmEDCRequestHead
  { requestTimeStamp :: Text, -- Format: "yyyy-MM-dd HH:mm:ss"
    channelId :: Text,
    checksum :: Text,
    version :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Merchant Extended Info for controlling EDC behavior
data MerchantExtendedInfo = MerchantExtendedInfo
  { autoAccept :: Maybe Text, -- "True" for auto-accept
    paymentMode :: Maybe Text -- "QR", "CARD", "ALL"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Sale Request Body
data PaytmEDCSaleRequestBody = PaytmEDCSaleRequestBody
  { paytmMid :: Text,
    paytmTid :: Maybe Text, -- Terminal ID
    transactionDateTime :: Text,
    merchantTransactionId :: Text,
    merchantReferenceNo :: Maybe Text,
    transactionAmount :: Text,
    merchantExtendedInfo :: Maybe MerchantExtendedInfo,
    callbackUrl :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Sale Request
data PaytmEDCSaleRequest = PaytmEDCSaleRequest
  { saleRequestHead :: PaytmEDCRequestHead,
    saleRequestBody :: PaytmEDCSaleRequestBody
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PaytmEDCSaleRequest where
  toJSON req =
    object
      [ "head" .= saleRequestHead req,
        "body" .= saleRequestBody req
      ]

instance FromJSON PaytmEDCSaleRequest where
  parseJSON = withObject "PaytmEDCSaleRequest" $ \v ->
    PaytmEDCSaleRequest
      <$> v .: "head"
      <*> v .: "body"

-- Status Enquiry Request Body
data PaytmEDCStatusRequestBody = PaytmEDCStatusRequestBody
  { paytmMid :: Text,
    paytmTid :: Maybe Text,
    merchantTransactionId :: Text,
    transactionDateTime :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Status Enquiry Request
data PaytmEDCStatusRequest = PaytmEDCStatusRequest
  { statusRequestHead :: PaytmEDCRequestHead,
    statusRequestBody :: PaytmEDCStatusRequestBody
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PaytmEDCStatusRequest where
  toJSON req =
    object
      [ "head" .= statusRequestHead req,
        "body" .= statusRequestBody req
      ]

instance FromJSON PaytmEDCStatusRequest where
  parseJSON = withObject "PaytmEDCStatusRequest" $ \v ->
    PaytmEDCStatusRequest
      <$> v .: "head"
      <*> v .: "body"

-- Response Result Info
data PaytmEDCResultInfo = PaytmEDCResultInfo
  { resultStatus :: Text, -- "A" (Accepted), "F" (Failed), "P" (Pending)
    resultCode :: Text,
    resultMsg :: Text,
    resultCodeId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Response Head
data PaytmEDCResponseHead = PaytmEDCResponseHead
  { responseTimeStamp :: Maybe Text,
    channelId :: Maybe Text,
    version :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Response Body
data PaytmEDCResponseBody = PaytmEDCResponseBody
  { merchantTransactionId :: Maybe Text,
    paytmMid :: Maybe Text,
    paytmTid :: Maybe Text,
    transactionAmount :: Maybe Text,
    resultInfo :: PaytmEDCResultInfo,
    txnId :: Maybe Text, -- Paytm transaction ID
    bankTxnId :: Maybe Text, -- Bank reference number
    paymentMode :: Maybe Text -- Mode used for payment
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Sale/Status Response
data PaytmEDCResponse = PaytmEDCResponse
  { responseHead :: PaytmEDCResponseHead,
    responseBody :: PaytmEDCResponseBody
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PaytmEDCResponse where
  toJSON resp =
    object
      [ "head" .= responseHead resp,
        "body" .= responseBody resp
      ]

instance FromJSON PaytmEDCResponse where
  parseJSON = withObject "PaytmEDCResponse" $ \v ->
    PaytmEDCResponse
      <$> v .: "head"
      <*> v .: "body"

-- Paytm EDC Transaction Status
data PaytmEDCStatus
  = EDC_ACCEPTED -- "A"
  | EDC_FAILED -- "F"
  | EDC_PENDING -- "P"
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Parse Paytm status code to our status
parsePaytmStatus :: Text -> PaytmEDCStatus
parsePaytmStatus "A" = EDC_ACCEPTED
parsePaytmStatus "F" = EDC_FAILED
parsePaytmStatus _ = EDC_PENDING

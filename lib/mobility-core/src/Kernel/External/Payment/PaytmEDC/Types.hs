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
import qualified Data.Text as T
import Kernel.Prelude

-- Helper function to remove hyphens from text
removeHyphens :: Text -> Text
removeHyphens = T.replace "-" ""

-- Req head for both sale and status enquiry
data PaytmEDCRequestHead = PaytmEDCRequestHead
  { requestTimeStamp :: Text, -- Format: "yyyy-MM-dd HH:mm:ss"
    channelId :: Text,
    checksum :: Text
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
    transactionAmount :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance ToJSON PaytmEDCSaleRequestBody where
  toJSON (PaytmEDCSaleRequestBody paytmMid paytmTid transactionDateTime merchantTransactionId merchantReferenceNo transactionAmount) =
    object $
      [ "paytmMid" .= paytmMid,
        "transactionDateTime" .= transactionDateTime,
        "merchantTransactionId" .= removeHyphens merchantTransactionId,
        "transactionAmount" .= transactionAmount
      ]
        <> maybe [] (\tid -> ["paytmTid" .= tid]) paytmTid
        <> maybe [] (\mrn -> ["merchantReferenceNo" .= removeHyphens mrn]) merchantReferenceNo

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
  deriving anyclass (FromJSON)

instance ToJSON PaytmEDCStatusRequestBody where
  toJSON (PaytmEDCStatusRequestBody paytmMid paytmTid merchantTransactionId transactionDateTime) =
    object $
      [ "paytmMid" .= paytmMid,
        "merchantTransactionId" .= removeHyphens merchantTransactionId,
        "transactionDateTime" .= transactionDateTime
      ]
        <> maybe [] (\tid -> ["paytmTid" .= tid]) paytmTid

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

-- Response Body (for QR and Card payments only)
data PaytmEDCResponseBody = PaytmEDCResponseBody
  { -- Basic transaction info
    merchantTransactionId :: Maybe Text,
    merchantReferenceNo :: Maybe Text,
    paytmMid :: Maybe Text,
    paytmTid :: Maybe Text,
    transactionDateTime :: Maybe Text,
    transactionAmount :: Maybe Text,
    -- Result info
    resultInfo :: PaytmEDCResultInfo,
    -- Paytm IDs
    acquirementId :: Maybe Text, -- Paytm internal transaction ID
    txnId :: Maybe Text,
    -- Bank/Acquirer info
    bankTxnId :: Maybe Text, -- Bank transaction ID
    retrievalReferenceNo :: Maybe Text, -- RRN
    authCode :: Maybe Text, -- Authorization code
    bankResponseCode :: Maybe Text,
    bankResponseMessage :: Maybe Text,
    bankMid :: Maybe Text,
    bankTid :: Maybe Text,
    acquiringBank :: Maybe Text,
    -- Card info (for card payments)
    issuerMaskCardNo :: Maybe Text, -- Masked card number
    issuingBankName :: Maybe Text,
    cardType :: Maybe Text, -- "CREDIT_CARD", "DEBIT_CARD"
    cardScheme :: Maybe Text, -- "VISA", "MASTERCARD", etc.
    aid :: Maybe Text, -- Application ID (EMV)
    -- Payment method
    payMethod :: Maybe Text, -- "CARD", "UPI", etc.
    paymentMode :: Maybe Text
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

-- Paytm EDC Result Codes (from resultCodeId field)
data PaytmEDCResultCode
  = EDC_SUCCESS -- 0000/0009: SUCCESS/ACCEPTED_SUCCESS
  -- Pending states (status enquiry)
  | EDC_PENDING_CHECK_MACHINE -- 0010/0030: Please check status on EDC machine
  -- Failure states
  | EDC_INVALID_CHECKSUM -- 0330: Invalid checksum
  | EDC_SUBWALLET_AMOUNT_ERROR -- 1809: Subwallet amount greater than transaction amount
  | EDC_DISPLAY_INFO_LENGTH_ERROR -- 1810: Length of DisplayInfo greater than max
  | EDC_TERMINAL_NOT_ACTIVE -- 0007: Terminal is not in active state
  | EDC_MULTIPLE_PAYMENT_REQUEST -- 0333: Multiple payment request not allowed
  | EDC_INVALID_PARAMETERS -- 0002: Request parameters are not valid
  | EDC_SYSTEM_ERROR -- 0012: Internal server error
  | EDC_BLOCKED_MERCHANT -- 0022: Blocked merchant
  | EDC_NOT_FOUND -- 0029: Not found
  | EDC_TXN_NOT_FOUND -- 0404: Merchant transaction id does not exist
  | EDC_SALE_FAILED -- 0011: The sale txn has failed
  | EDC_DUPLICATE_ORDER -- 0233: Duplicate Order Id
  | EDC_MERCHANT_CONFIG_ERROR -- 9001: Merchant configuration error
  | EDC_MERCHANT_BLOCKED -- 0902: Merchant blocked
  | EDC_VOID_FAILED -- 0090: ECR void failed
  | EDC_VOID_NOT_FOUND -- 0180: VOID transaction not found
  | EDC_INVALID_SPLIT_TYPE -- 1825: Invalid splitType
  | EDC_TIMEOUT_CONFIG_WITH_SPLIT -- 1815: Timeout config cannot be passed with split params
  | EDC_SPLIT_PARAMS_POST_FACTO -- 1813: Split params cannot be passed with POST_FACTO type
  | EDC_TIMEOUT_CONFIG_AT_TXN -- 1814: Timeout config with AT_TXN time
  | EDC_UNKNOWN_ERROR -- Any other error
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Parse resultCodeId to PaytmEDCResultCode
parseResultCodeId :: Maybe Text -> PaytmEDCResultCode
-- Success
parseResultCodeId (Just "0000") = EDC_SUCCESS
parseResultCodeId (Just "0009") = EDC_SUCCESS
-- Pending
parseResultCodeId (Just "0010") = EDC_PENDING_CHECK_MACHINE
parseResultCodeId (Just "0030") = EDC_PENDING_CHECK_MACHINE
-- Failures
parseResultCodeId (Just "0011") = EDC_SALE_FAILED
parseResultCodeId (Just "0330") = EDC_INVALID_CHECKSUM
parseResultCodeId (Just "0404") = EDC_TXN_NOT_FOUND
parseResultCodeId (Just "0090") = EDC_VOID_FAILED
parseResultCodeId (Just "0180") = EDC_VOID_NOT_FOUND
parseResultCodeId (Just "0012") = EDC_SYSTEM_ERROR
parseResultCodeId (Just "1809") = EDC_SUBWALLET_AMOUNT_ERROR
parseResultCodeId (Just "1810") = EDC_DISPLAY_INFO_LENGTH_ERROR
parseResultCodeId (Just "0007") = EDC_TERMINAL_NOT_ACTIVE
parseResultCodeId (Just "0333") = EDC_MULTIPLE_PAYMENT_REQUEST
parseResultCodeId (Just "0002") = EDC_INVALID_PARAMETERS
parseResultCodeId (Just "0022") = EDC_BLOCKED_MERCHANT
parseResultCodeId (Just "0029") = EDC_NOT_FOUND
parseResultCodeId (Just "0233") = EDC_DUPLICATE_ORDER
parseResultCodeId (Just "9001") = EDC_MERCHANT_CONFIG_ERROR
parseResultCodeId (Just "0902") = EDC_MERCHANT_BLOCKED
parseResultCodeId (Just "1825") = EDC_INVALID_SPLIT_TYPE
parseResultCodeId (Just "1815") = EDC_TIMEOUT_CONFIG_WITH_SPLIT
parseResultCodeId (Just "1813") = EDC_SPLIT_PARAMS_POST_FACTO
parseResultCodeId (Just "1814") = EDC_TIMEOUT_CONFIG_AT_TXN
parseResultCodeId _ = EDC_UNKNOWN_ERROR

-- Check if result code indicates success
isSuccessCode :: PaytmEDCResultCode -> Bool
isSuccessCode EDC_SUCCESS = True
isSuccessCode _ = False

-- Get error message for result code
getResultCodeMessage :: PaytmEDCResultCode -> Text
getResultCodeMessage EDC_SUCCESS = "Payment successful"
getResultCodeMessage EDC_PENDING_CHECK_MACHINE = "Please check status on EDC machine"
getResultCodeMessage EDC_INVALID_CHECKSUM = "Invalid checksum"
getResultCodeMessage EDC_SUBWALLET_AMOUNT_ERROR = "Subwallet amount greater than transaction amount"
getResultCodeMessage EDC_DISPLAY_INFO_LENGTH_ERROR = "DisplayInfo parameter exceeds maximum length"
getResultCodeMessage EDC_TERMINAL_NOT_ACTIVE = "Terminal is not in active state"
getResultCodeMessage EDC_MULTIPLE_PAYMENT_REQUEST = "Multiple payment request not allowed for same terminal"
getResultCodeMessage EDC_INVALID_PARAMETERS = "Request parameters are not valid"
getResultCodeMessage EDC_SYSTEM_ERROR = "Internal server error. Please retry"
getResultCodeMessage EDC_BLOCKED_MERCHANT = "Blocked merchant"
getResultCodeMessage EDC_NOT_FOUND = "Not found"
getResultCodeMessage EDC_TXN_NOT_FOUND = "Merchant transaction id does not exist"
getResultCodeMessage EDC_SALE_FAILED = "The sale transaction has failed"
getResultCodeMessage EDC_DUPLICATE_ORDER = "Duplicate Order Id"
getResultCodeMessage EDC_MERCHANT_CONFIG_ERROR = "Merchant configuration error"
getResultCodeMessage EDC_MERCHANT_BLOCKED = "Merchant blocked"
getResultCodeMessage EDC_VOID_FAILED = "ECR void failed"
getResultCodeMessage EDC_VOID_NOT_FOUND = "VOID transaction not found"
getResultCodeMessage EDC_INVALID_SPLIT_TYPE = "Invalid split type"
getResultCodeMessage EDC_TIMEOUT_CONFIG_WITH_SPLIT = "Timeout config cannot be passed with split params"
getResultCodeMessage EDC_SPLIT_PARAMS_POST_FACTO = "Split params cannot be passed with POST_FACTO type"
getResultCodeMessage EDC_TIMEOUT_CONFIG_AT_TXN = "Timeout config cannot be passed with AT_TXN time"
getResultCodeMessage EDC_UNKNOWN_ERROR = "Unknown error"

-- Simple status for backward compatibility
data PaytmEDCStatus
  = EDC_ACCEPTED_SUCCESS
  | EDC_FAILED
  | EDC_PENDING
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Convert result code to simple status
resultCodeToStatus :: PaytmEDCResultCode -> PaytmEDCStatus
resultCodeToStatus EDC_SUCCESS = EDC_ACCEPTED_SUCCESS
resultCodeToStatus EDC_PENDING_CHECK_MACHINE = EDC_PENDING
resultCodeToStatus EDC_UNKNOWN_ERROR = EDC_PENDING
resultCodeToStatus _ = EDC_FAILED

-- Parse resultStatus (legacy - use parseResultCodeId for detailed info)
parsePaytmStatus :: Text -> PaytmEDCStatus
parsePaytmStatus "ACCEPTED_SUCCESS" = EDC_ACCEPTED_SUCCESS
parsePaytmStatus "ACCEPTED" = EDC_ACCEPTED_SUCCESS
parsePaytmStatus "FAIL" = EDC_FAILED
parsePaytmStatus _ = EDC_PENDING

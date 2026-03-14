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

module Kernel.External.Payment.Juspay.Types.Common where

import Control.Lens
import Data.Aeson.Types
import Data.OpenApi hiding (components, description, links, name)
import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common
import Kernel.Utils.JSON (camelToSnakeCase)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

type CustomerId = Text

data PaymentStatus
  = ORDER_SUCCEEDED
  | ORDER_REFUNDED
  | ORDER_FAILED
  | ORDER_REFUND_FAILED
  | TXN_CREATED
  | REFUND_MANUAL_REVIEW_NEEDED
  | REFUND_INITIATED
  | AUTO_REFUND_INITIATED
  | AUTO_REFUND_SUCCEEDED
  | AUTO_REFUND_FAILED
  | MANDATE_CREATED
  | MANDATE_ACTIVATED
  | MANDATE_FAILED
  | MANDATE_REVOKED
  | MANDATE_PAUSED
  | MANDATE_EXPIRED
  | NOTIFICATION_FAILED
  | NOTIFICATION_SUCCEEDED
  | ORDER_AUTHORIZED
  | TXN_CHARGED
  | TXN_FAILED
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateType = OPTIONAL | REQUIRED
  deriving stock (Show, Read, Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

derivePersistField "MandateType"

$(mkBeamInstancesForEnum ''MandateType)

data MandateFrequency = ONETIME | DAILY | WEEKLY | FORTNIGHTLY | MONTHLY | BIMONTHLY | QUARTERLY | HALFYEARLY | YEARLY | ASPRESENTED
  deriving stock (Show, Read, Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''MandateFrequency)

derivePersistField "MandateFrequency"

data MandateStatus = CREATED | ACTIVE | FAILURE | PAUSED | EXPIRED | REVOKED
  deriving stock (Show, Read, Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''MandateStatus)

derivePersistField "MandateStatus"

data NotificationStatus = NOTIFICATION_CREATED | NOTIFICATION_FAILURE | PENDING | SUCCESS
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

derivePersistField "NotificationStatus"

instance FromJSON NotificationStatus where
  parseJSON (String "CREATED") = pure NOTIFICATION_CREATED
  parseJSON (String "FAILURE") = pure NOTIFICATION_FAILURE
  parseJSON (String "PENDING") = pure PENDING
  parseJSON (String "SUCCESS") = pure SUCCESS
  parseJSON (String _) = parseFail "Expected type"
  parseJSON e = typeMismatch "String" e

instance ToJSON NotificationStatus where
  toJSON NOTIFICATION_CREATED = "CREATED"
  toJSON NOTIFICATION_FAILURE = "FAILURE"
  toJSON PENDING = "PENDING"
  toJSON SUCCESS = "SUCCESS"

data RefundStatus = REFUND_PENDING | REFUND_FAILURE | REFUND_SUCCESS | MANUAL_REVIEW
  deriving stock (Show, Eq, Read, Ord, Generic)

instance FromJSON RefundStatus where
  parseJSON (String "FAILURE") = pure REFUND_FAILURE
  parseJSON (String "PENDING") = pure REFUND_PENDING
  parseJSON (String "SUCCESS") = pure REFUND_SUCCESS
  parseJSON (String "MANUAL_REVIEW") = pure MANUAL_REVIEW
  parseJSON (String _) = parseFail "Expected type"
  parseJSON e = typeMismatch "String" e

instance ToJSON RefundStatus where
  toJSON REFUND_FAILURE = "FAILURE"
  toJSON REFUND_PENDING = "PENDING"
  toJSON REFUND_SUCCESS = "SUCCESS"
  toJSON MANUAL_REVIEW = "MANUAL_REVIEW"

instance ToSchema RefundStatus where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "RefundStatus") $
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ (map toJSON [REFUND_PENDING, REFUND_FAILURE, REFUND_SUCCESS, MANUAL_REVIEW])

data OfferState = OFFER_INITIATED | OFFER_AVAILED | OFFER_REFUNDED | OFFER_FAILED
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

derivePersistField "OfferState"

instance FromJSON OfferState where
  parseJSON (String "INITIATED") = pure OFFER_INITIATED
  parseJSON (String "AVAILED") = pure OFFER_AVAILED
  parseJSON (String "REFUNDED") = pure OFFER_REFUNDED
  parseJSON (String "FAILED") = pure OFFER_FAILED
  parseJSON (String _) = parseFail "Expected type"
  parseJSON e = typeMismatch "String" e

instance ToJSON OfferState where
  toJSON OFFER_INITIATED = "INITIATED"
  toJSON OFFER_AVAILED = "AVAILED"
  toJSON OFFER_REFUNDED = "REFUNDED"
  toJSON OFFER_FAILED = "FAILED"

data TransactionStatus
  = NEW
  | PENDING_VBV
  | CHARGED
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | CANCELLED -- for stripe
  | JUSPAY_DECLINED
  | AUTHORIZING
  | COD_INITIATED
  | STARTED
  | AUTO_REFUNDED
  | CLIENT_AUTH_TOKEN_EXPIRED -- Domain status, not part of Juspay Euler status types
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)

derivePersistField "TransactionStatus"

$(mkBeamInstancesForEnum ''TransactionStatus)

instance FromHttpApiData TransactionStatus where
  parseUrlPiece txt = left (\_err -> "Invalid TransactionStatus: " <> txt) . readEither . T.toUpper $ txt

instance ToHttpApiData TransactionStatus where
  toUrlPiece = T.pack . show

type OrderStatusResp = OrderData

jsonSnakeOptions :: Options
jsonSnakeOptions = defaultOptions {fieldLabelModifier = camelToSnakeCase}

data LoyaltyBurnApplicable = LoyaltyBurnApplicable
  { points :: Text,
    discountAmount :: Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyBurnApplicable where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyBurnApplicable where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyBurnIneligibleReason = LoyaltyBurnIneligibleReason
  { message :: Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyBurnIneligibleReason where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyBurnIneligibleReason where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyBurnOptionSelected = LoyaltyBurnOptionSelected
  { id :: Text,
    pointsSelected :: Text,
    status :: Text,
    ineligibleReasons :: Maybe [LoyaltyBurnIneligibleReason],
    applicable :: Maybe LoyaltyBurnApplicable
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyBurnOptionSelected where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyBurnOptionSelected where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyBurnDetail = LoyaltyBurnDetail
  { programId :: Text,
    burnOptionsSelected :: Maybe [LoyaltyBurnOptionSelected]
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyBurnDetail where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyBurnDetail where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyEarnApplied = LoyaltyEarnApplied
  { points :: Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyEarnApplied where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyEarnApplied where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyEarnCampaign = LoyaltyEarnCampaign
  { id :: Text,
    applied :: Maybe LoyaltyEarnApplied
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyEarnCampaign where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyEarnCampaign where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyEarnDetail = LoyaltyEarnDetail
  { programId :: Text,
    applied :: Maybe LoyaltyEarnApplied,
    campaigns :: Maybe [LoyaltyEarnCampaign]
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyEarnDetail where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyEarnDetail where
  toJSON = genericToJSON jsonSnakeOptions

data LoyaltyInfo = LoyaltyInfo
  { burnDetails :: Maybe [LoyaltyBurnDetail],
    earnDetails :: Maybe [LoyaltyEarnDetail]
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyInfo where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyInfo where
  toJSON = genericToJSON jsonSnakeOptions

data TxnAmountBreakup = TxnAmountBreakup
  { amount :: Double,
    method :: Text,
    sno :: Text,
    name :: Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON TxnAmountBreakup where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON TxnAmountBreakup where
  toJSON = genericToJSON jsonSnakeOptions

data TxnDetail = TxnDetail
  { txnId :: Maybe Text,
    orderId :: Maybe Text,
    errorCode :: Maybe Text,
    txnFlowType :: Maybe Text,
    merchantIdentifier :: Maybe Text,
    status :: Maybe TransactionStatus,
    expressCheckout :: Maybe Bool,
    gatewayId :: Maybe Int,
    errorMessage :: Maybe Text,
    offerDeductionAmount :: Maybe Double,
    taxAmount :: Maybe HighPrecMoney,
    currency :: Maybe Currency,
    txnAmountBreakup :: Maybe [TxnAmountBreakup],
    txnAmount :: Maybe Double,
    loyaltyInfo :: Maybe LoyaltyInfo,
    created :: Maybe UTCTime,
    gateway :: Maybe Text,
    surchargeAmount :: Maybe HighPrecMoney,
    lastUpdated :: Maybe UTCTime,
    txnUuid :: Maybe Text,
    netAmount :: Maybe HighPrecMoney,
    redirect :: Maybe Bool,
    metadata :: Maybe Object,
    isCvvLessTxn :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance Read TxnDetail where
  readsPrec _ _ = []

instance FromJSON TxnDetail where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON TxnDetail where
  toJSON = genericToJSON jsonSnakeOptions

data TxnList = TxnList
  { paymentMethod :: Maybe Text,
    txnId :: Maybe Text,
    gatewayReferenceId :: Maybe Text,
    txnDetail :: Maybe TxnDetail,
    status :: Maybe TransactionStatus,
    respMessage :: Maybe Text,
    refunded :: Maybe Bool,
    gatewayId :: Maybe Int,
    effectiveAmount :: Maybe Double,
    authType :: Maybe Text,
    respCode :: Maybe Text,
    card :: Maybe Text,
    paymentMethodType :: Maybe Text,
    txnUuid :: Maybe Text,
    respCategory :: Maybe Text,
    bankErrorCode :: Maybe Text,
    emiDetails :: Maybe Text,
    bankErrorMessage :: Maybe Text,
    s :: Maybe [Text]
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON TxnList where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON TxnList where
  toJSON = genericToJSON jsonSnakeOptions

data OrderStatusWithLoyaltyResp = OrderStatusWithLoyaltyResp
  { txnList :: Maybe [TxnList],
    customerPhone :: Maybe Text,
    orderId :: Text,
    customerId :: Maybe Text,
    amount :: Double,
    status :: TransactionStatus,
    dateCreated :: Maybe UTCTime,
    id :: Text,
    merchantId :: Maybe Text,
    returnUrl :: Maybe Text,
    effectiveAmount :: Maybe Double,
    currency :: Currency,
    loyaltyInfo :: Maybe LoyaltyInfo,
    customerEmail :: Maybe Text,
    customerPhoneCountryCode :: Maybe Text,
    lastUpdated :: Maybe UTCTime,
    amountRefunded :: Maybe Double
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON OrderStatusWithLoyaltyResp where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON OrderStatusWithLoyaltyResp where
  toJSON = genericToJSON jsonSnakeOptions

data OrderData = OrderData
  { order_id :: Text,
    txn_uuid :: Maybe Text,
    txn_id :: Maybe Text,
    status_id :: Maybe Int,
    event_name :: Maybe PaymentStatus, --- only webhook res contains this field ---
    status :: TransactionStatus,
    payment_method_type :: Maybe Text,
    payment_method :: Maybe Text,
    payment_gateway_response :: Maybe PaymentGatewayResponse,
    resp_message :: Maybe Text,
    resp_code :: Maybe Text,
    gateway_reference_id :: Maybe Text,
    amount :: Double,
    currency :: Currency,
    date_created :: Maybe UTCTime,
    mandate :: Maybe MandateData,
    payer_vpa :: Maybe Text,
    bank_error_code :: Maybe Text,
    bank_error_message :: Maybe Text,
    upi :: Maybe Upi,
    card :: Maybe CardInfo,
    metadata :: Maybe MetaData,
    additional_info :: Maybe AdditionalInfo,
    links :: Maybe LinkData,
    amount_refunded :: Maybe Double,
    refunds :: Maybe [RefundsData],
    split_settlement_response :: Maybe SplitSettlementResponse,
    effective_amount :: Maybe Double,
    offers :: Maybe [Offer],
    txn_detail :: Maybe TxnDetail,
    loyalty_info :: Maybe LoyaltyInfo,
    txn_list :: Maybe [TxnList]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Offer = Offer
  { offer_id :: Maybe Text,
    offer_code :: Maybe Text,
    status :: OfferState
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, ToSchema, FromJSON)

data SplitSettlementResponse = SplitSettlementResponse
  { split_details :: Maybe [SplitDetailsResponse],
    split_applied :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SplitDetailsResponse = SplitDetailsResponse
  { sub_vendor_id :: Maybe Text,
    amount :: Maybe HighPrecMoney,
    merchant_commission :: Maybe HighPrecMoney,
    gateway_sub_account_id :: Maybe Text,
    epg_txn_id :: Maybe Text,
    unique_split_id :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PaymentGatewayResponse = PaymentGatewayResponse
  { resp_code :: Maybe Text,
    rrn :: Maybe Text,
    created :: Maybe UTCTime,
    epg_txn_id :: Maybe Text,
    resp_message :: Maybe Text,
    auth_id_code :: Maybe Text,
    txn_id :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- data TxnDetail = TxnDetail
--   { gateway :: Maybe Text,
--     surcharge_amount :: Maybe Double,
--     tax_amount :: Maybe Double,
--     net_amount :: Maybe Double
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateData = MandateData
  { mandate_status :: MandateStatus,
    start_date :: Text,
    end_date :: Text,
    mandate_id :: Text,
    frequency :: MandateFrequency,
    max_amount :: HighPrecMoney
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Upi = Upi
  { payer_app :: Maybe Text,
    payer_app_name :: Maybe Text,
    payer_vpa :: Maybe Text,
    txn_flow_type :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CardInfo = CardInfo
  { card_type :: Maybe Text,
    last_four_digits :: Maybe Text,
    name_on_card :: Maybe Text,
    card_brand :: Maybe Text,
    card_isin :: Maybe Text,
    card_issuer :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MetaData = MetaData
  { is_retried_order :: Maybe Text,
    juspay_internal_retarget_configs :: Maybe RetargetConfigs
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RetargetConfigs = RetargetConfigs
  { retarget_payment_link_expiry :: Maybe Text,
    is_retargeted_order :: Maybe Text,
    retarget_done_count :: Maybe Text,
    max_retarget_limit :: Maybe Text,
    retarget_payment_link :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype LinkData = LinkData
  { retarget_payment_links :: Maybe RetargetConfigs
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdditionalInfo = AdditionalInfo
  { retarget_payment_info :: Maybe RetargetConfigs,
    mandate_retry_info :: Maybe MandateRetryInfo,
    is_business_retried :: Maybe Text,
    is_technical_retried :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MandateRetryInfo = MandateRetryInfo
  { is_retried_order :: Maybe Text,
    retries_done_count :: Maybe Text,
    is_technical_retried :: Maybe Text,
    allowed_retry :: Maybe Text,
    is_business_retried :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RefundsData = RefundsData
  { id :: Maybe Text,
    amount :: Double,
    status :: RefundStatus,
    error_message :: Maybe Text,
    error_code :: Maybe Text,
    initiated_by :: Maybe Text,
    unique_request_id :: Text,
    arn :: Maybe Text
  }
  deriving stock (Show, Generic, Read, Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data VerifyVPAReq = VerifyVPAReq
  { vpa :: Text,
    merchant_id :: Text,
    customer_id :: Maybe Text,
    order_id :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype MandateDetails = MandateDetails
  { is_handle_supported :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VPAStatus = VALID | INVALID
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

derivePersistField "VPAStatus"

$(mkBeamInstancesForEnum ''VPAStatus)

data VerifyVPAResp = VerifyVPAResp
  { vpa :: Text,
    status :: VPAStatus,
    mandate_details :: Maybe MandateDetails,
    customer_name :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

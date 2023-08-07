{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Types.Offer where

import Data.Aeson
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

-- offer list request --

data OfferListReq = OfferListReq
  { order :: OfferOrder,
    payment_method_info :: [OfferPaymentMethodInfo],
    customer :: Maybe OfferCustomer,
    offer_code :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferOrder = OfferOrder
  { order_id :: Maybe Text,
    amount :: Text,
    currency :: Currency
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferPaymentMethodInfo = OfferPaymentMethodInfo
  { payment_method_type :: PaymentMethodType,
    payment_method_reference :: Maybe Text,
    payment_method :: Maybe Text,
    card_number :: Maybe Text,
    card_token :: Maybe Text,
    txn_type :: Maybe TxnType,
    upi_app :: Maybe Text,
    upi_vpa :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PaymentMethodType = CARD | NB | UPI | WALLET | REWARD
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TxnType = UPI_PAY | UPI_COLLECT
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferCustomer = OfferCustomer
  { id :: Text,
    email :: Maybe Text,
    mobile :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- offer list response --

data OfferListResp = OfferListResp
  { best_offer_combinations :: [BestOfferCombination],
    offers :: [OfferResp]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BestOfferCombination = BestOfferCombination
  { payment_method_reference :: Maybe Text,
    offers :: [BestOfferCombinationOffer],
    order_breakup :: OrderBreakup
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BestOfferCombinationOffer = BestOfferCombinationOffer
  { offer_id :: Text,
    cashback_amount :: Text,
    discount_amount :: Text,
    merchant_discount_amount :: Text,
    total_offered_amount :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OrderBreakup = OrderBreakup
  { order_amount :: Text,
    final_order_amount :: Text,
    discount_amount :: Text,
    merchant_discount_amount :: Text,
    cashback_amount :: Text,
    offer_amount :: Text,
    benefits :: [BestOfferCombinationBenefit]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BestOfferCombinationBenefit = BestOfferCombinationBenefit
  { _type :: Text,
    calculation_rule :: Text,
    value :: HighPrecMoney
  }
  deriving stock (Show, Generic)

instance FromJSON BestOfferCombinationBenefit where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON BestOfferCombinationBenefit where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data OfferResp = OfferResp
  { offer_id :: Text,
    status :: OfferStatus,
    offer_code :: Text,
    offer_description :: OfferDescription,
    offer_rules :: OfferRules,
    order_breakup :: OrderBreakup
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferDescription = OfferDescription
  { sponsored_by :: Maybe Text,
    title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferRules = OfferRules
  { amount :: OfferRulesAmount,
    payment_instrument :: OfferRulesPaymentInstrument
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferRulesAmount = OfferRulesAmount
  { currency :: Currency,
    min_order_amount :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferRulesPaymentInstrument = OfferRulesPaymentInstrument
  { payment_method :: [Text],
    payment_method_type :: PaymentMethodType
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- offer apply request --

data OfferApplyReq = OfferApplyReq
  { customer :: OfferApplyCustomer,
    offers :: [Text],
    order :: OfferApplyOrder,
    payment_method_info :: Maybe OfferApplyPaymentMethodInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferApplyOrder = OfferApplyOrder
  { order_id :: Text,
    amount :: Text,
    currency :: Currency,
    merchant_id :: Maybe Text,
    order_type :: Maybe Text,
    udf1 :: Maybe Text,
    payment_channel :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferApplyPaymentMethodInfo = OfferApplyPaymentMethodInfo
  { payment_method_type :: PaymentMethodType,
    payment_method_reference :: Maybe Text,
    payment_method :: Maybe Text,
    card_type :: Maybe Text,
    card_sub_type :: Maybe Text,
    bank_code :: Maybe Text,
    card_bin :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype OfferApplyCustomer = OfferApplyCustomer
  { id :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- offer apply response (other fields omitted) --

newtype OfferApplyResp = OfferApplyResp
  { offers :: [OfferApplyOffer]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferApplyOffer = OfferApplyOffer
  { offer_id :: Text,
    order_breakup :: OfferApplyOrderBreakup
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype OfferApplyOrderBreakup = OfferApplyOrderBreakup
  { final_order_amount :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- offer notify request --

data OfferNotifyReq = OfferNotifyReq
  { order_id :: Text,
    txn_id :: Text,
    merchant_id :: Text,
    txn_status :: TransactionStatus,
    offers :: [OfferNotifyOffer]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferNotifyOffer = OfferNotifyOffer
  { offer_id :: Text,
    status :: OfferStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OfferStatus = INITIATED | AVAILED | REFUNDED | FAILED
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- offer notify response --

data OfferNotifyResp = OfferNotifyResp
  { code :: Text,
    status :: Text,
    response :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

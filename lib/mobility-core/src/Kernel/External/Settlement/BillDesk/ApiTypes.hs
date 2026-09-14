{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.ApiTypes
  ( RetrieveSettlementReq (..),
    AmountDetails (..),
    SettlementObj (..),
    RetrieveSettlementDetailsReq (..),
    AdditionalInfo (..),
    SettlementRecord (..),
    LinkObj (..),
    LinkParams (..),
    SettlementDetailsResp (..),
    BillDeskError (..),
  )
where

import Kernel.Prelude

-- ---------------------------------------------------------------------------
-- Retrieve Settlement API
-- ---------------------------------------------------------------------------

data RetrieveSettlementReq = RetrieveSettlementReq
  { mercid :: Text,
    from_date :: Maybe Text,
    to_date :: Maybe Text,
    pv_number :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AmountDetails = AmountDetails
  { settlement :: Maybe Text,
    refund :: Maybe Text,
    chargeback :: Maybe Text,
    refund_reversal :: Maybe Text,
    chargeback_reversal :: Maybe Text,
    adjustment :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SettlementObj = SettlementObj
  { objectid :: Maybe Text,
    pv_number :: Text,
    mercid :: Text,
    payout_mercid :: Maybe Text,
    pv_file :: Maybe Text,
    pv_file_date :: Maybe Text,
    currency :: Maybe Text,
    amount_details :: Maybe AmountDetails,
    charges :: Maybe Text,
    taxes :: Maybe Text,
    other_adjustments :: Maybe Text,
    payout_amount :: Maybe Text,
    status :: Maybe Text,
    settlement_date :: Maybe Text,
    utr :: Maybe Text,
    utr_date :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Retrieve Settlement Details API
-- ---------------------------------------------------------------------------

data RetrieveSettlementDetailsReq = RetrieveSettlementDetailsReq
  { mercid :: Text,
    pv_number :: Text,
    page_number :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AdditionalInfo = AdditionalInfo
  { additional_info1 :: Maybe Text,
    additional_info2 :: Maybe Text,
    additional_info3 :: Maybe Text,
    additional_info4 :: Maybe Text,
    additional_info5 :: Maybe Text,
    additional_info6 :: Maybe Text,
    additional_info7 :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SettlementRecord = SettlementRecord
  { transaction_type :: Maybe Text,
    bankid :: Maybe Text,
    bank_ref_no :: Maybe Text,
    billdesk_id :: Maybe Text,
    merc_ref_id :: Maybe Text,
    date :: Maybe Text,
    settlement_date :: Maybe Text,
    amount :: Maybe Text,
    gross_amount :: Maybe Text,
    charges :: Maybe Text,
    taxes :: Maybe Text,
    net_amount :: Maybe Text,
    authcode :: Maybe Text,
    payment_category :: Maybe Text,
    settlement_ref1 :: Maybe Text,
    reference_id :: Maybe Text,
    reference_date :: Maybe Text,
    reference_amount :: Maybe Text,
    card_type :: Maybe Text,
    network :: Maybe Text,
    issuer :: Maybe Text,
    currency :: Maybe Text,
    additional_info :: Maybe AdditionalInfo
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LinkParams = LinkParams
  { mercid :: Text,
    pv_number :: Text,
    page_number :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LinkObj = LinkObj
  { href :: Maybe Text,
    rel :: Text,
    method :: Maybe Text,
    parameters :: Maybe LinkParams
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SettlementDetailsResp = SettlementDetailsResp
  { objectid :: Maybe Text,
    pv_number :: Maybe Text,
    mercid :: Maybe Text,
    payout_mercid :: Maybe Text,
    pv_file_date :: Maybe Text,
    page_total :: Int,
    page_number :: Int,
    page_record_count :: Maybe Int,
    total_record_count :: Maybe Int,
    records :: [SettlementRecord],
    links :: Maybe [LinkObj]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Error response
-- ---------------------------------------------------------------------------

data BillDeskError = BillDeskError
  { status :: Maybe Int,
    error_type :: Maybe Text,
    error_code :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.CCAvenue.ApiTypes
  ( -- * API #22: getSettlementDetails
    SettlementDetailsReq (..),
    SettlementDetailsResp (..),
    SettlementDetailsResult (..),
    SettlementDetailsList (..),
    SettlementDetail (..),

    -- * API #20: payoutSummary
    PayoutSummaryReq (..),
    PayoutSummaryResp (..),
    PayoutSummaryResult (..),
    PayoutSummaryItem (..),

    -- * API #25: ConsolidateSettlementDetails
    ConsolidateSettlementReq (..),
    ConsolidateSettlementResp (..),
    ConsolidateSettlementResult (..),
    ConsolidateSettlementDetail (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.Char as Char
import Kernel.Prelude

-- ---------------------------------------------------------------------------
-- API #22: getSettlementDetails (v1.2)
-- ---------------------------------------------------------------------------

newtype SettlementDetailsReq = SettlementDetailsReq
  { reference_no :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype SettlementDetailsResp = SettlementDetailsResp
  { settlement_Details_Result :: SettlementDetailsResult
  }
  deriving (Show, Eq, Generic)

instance ToJSON SettlementDetailsResp where
  toJSON = A.genericToJSON capitalizeFirstOptions

instance FromJSON SettlementDetailsResp where
  parseJSON = A.genericParseJSON capitalizeFirstOptions

data SettlementDetailsResult = SettlementDetailsResult
  { error_code :: Maybe Text,
    error_desc :: Maybe Text,
    settlement_details_list :: Maybe SettlementDetailsList
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype SettlementDetailsList = SettlementDetailsList
  { settlement_details :: [SettlementDetail]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SettlementDetail = SettlementDetail
  { pay_Id :: Maybe Text,
    settlement_date :: Maybe Text,
    utr_no :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- API #20: payoutSummary (v1.1)
-- ---------------------------------------------------------------------------

newtype PayoutSummaryReq = PayoutSummaryReq
  { settlement_date :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype PayoutSummaryResp = PayoutSummaryResp
  { payout_Summary_Result :: PayoutSummaryResult
  }
  deriving (Show, Eq, Generic)

instance ToJSON PayoutSummaryResp where
  toJSON = A.genericToJSON capitalizeFirstOptions

instance FromJSON PayoutSummaryResp where
  parseJSON = A.genericParseJSON capitalizeFirstOptions

data PayoutSummaryResult = PayoutSummaryResult
  { error_code :: Maybe Text,
    error_desc :: Maybe Text,
    payout_summary :: Maybe [PayoutSummaryItem]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PayoutSummaryItem = PayoutSummaryItem
  { pay_Id :: Maybe Text,
    amount :: Maybe Text,
    utr_no :: Maybe Text,
    bank_name :: Maybe Text,
    settlement_date :: Maybe Text,
    currency :: Maybe Text,
    payout_status :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- API #25: ConsolidateSettlementDetails (version "DEF")
-- ---------------------------------------------------------------------------

data ConsolidateSettlementReq = ConsolidateSettlementReq
  { order_no :: Text,
    reference_no :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ConsolidateSettlementResp = ConsolidateSettlementResp
  { consolidate_Settlement_Details_Result :: ConsolidateSettlementResult
  }
  deriving (Show, Eq, Generic)

instance ToJSON ConsolidateSettlementResp where
  toJSON = A.genericToJSON capitalizeFirstOptions

instance FromJSON ConsolidateSettlementResp where
  parseJSON = A.genericParseJSON capitalizeFirstOptions

data ConsolidateSettlementResult = ConsolidateSettlementResult
  { error_code :: Maybe Text,
    error_desc :: Maybe Text,
    consolidate_settlement_details :: Maybe [ConsolidateSettlementDetail]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConsolidateSettlementDetail = ConsolidateSettlementDetail
  { pay_Id :: Maybe Text,
    settlement_date :: Maybe Text,
    utr_no :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Aeson options: capitalize first character of field names
-- ---------------------------------------------------------------------------

capitalizeFirstOptions :: A.Options
capitalizeFirstOptions =
  A.defaultOptions
    { A.fieldLabelModifier = capitalizeFirst
    }
  where
    capitalizeFirst [] = []
    capitalizeFirst (c : cs) = Char.toUpper c : cs

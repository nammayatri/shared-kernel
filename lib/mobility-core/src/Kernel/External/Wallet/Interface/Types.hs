{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  Copyright 2022-25, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.External.Wallet.Interface.Types
  ( module Kernel.External.Wallet.Interface.Types,
  )
where

import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Types.Common

data CreateWalletReq = CreateWalletReq
  { operationId :: Text,
    createCashAccount :: Bool,
    createPointsAccount :: Bool,
    customerId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateWalletResp = CreateWalletResp
  { success :: Bool,
    customerId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletPostingType = GRANT | REDEEM | REFUND
  deriving (Show, Eq, Read, Generic, Ord, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''WalletPostingType)

data WalletPostingReq = WalletPostingReq
  { customerId :: Text,
    postingType :: WalletPostingType,
    operationId :: Text,
    pointsAmount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletPostingResp = WalletPostingResp
  { success :: Bool,
    message :: Text,
    pointsAmount :: Int,
    cashAmount :: Int,
    operationId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletReversalReq = WalletReversalReq
  { customerId :: Text,
    operationId :: Text,
    txnOperationId :: Text,
    amount :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletReversalResp = WalletReversalResp
  { pointsAmount :: Int,
    success :: Bool,
    userId :: Maybe Text,
    cashAmount :: Int,
    udfParameters :: Maybe Value,
    message :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletBalanceReq = WalletBalanceReq
  { customerId :: Text,
    requireHistory :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletBalanceResp = WalletBalanceResp
  { success :: Bool,
    walletData :: WalletBalanceData
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletBalanceData = WalletBalanceData
  { openingBalance :: HighPrecMoney,
    closingBalance :: HighPrecMoney,
    expiredBalance :: HighPrecMoney,
    pointsAmount :: HighPrecMoney,
    usablePointsAmount :: HighPrecMoney,
    cashFromPointsRedemption :: HighPrecMoney,
    cashAmount :: HighPrecMoney,
    usableCashAmount :: HighPrecMoney,
    entries :: [WalletBalanceEntry],
    expiredEntries :: [WalletBalanceEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletBalanceEntry = WalletBalanceEntry
  { description :: Text,
    transactionType :: Text,
    earnedAt :: Maybe Text,
    expiresAt :: Maybe Text,
    entryType :: Text,
    amount :: HighPrecMoney,
    operationId :: Text,
    entityId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype WalletVerifyTxnReq = WalletVerifyTxnReq
  { operationId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletVerifyTxnResp = WalletVerifyTxnResp
  { operationId :: Text,
    success :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

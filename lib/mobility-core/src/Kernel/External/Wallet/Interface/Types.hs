{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.JSON (camelToSnakeCase)

-- | For serializing requests sent TO Juspay (snake_case)
jsonSnakeOptions :: Options
jsonSnakeOptions = defaultOptions {fieldLabelModifier = camelToSnakeCase}

-- | For serializing responses FROM Juspay to our consumers (camelCase).
-- Strips trailing underscores used to avoid Haskell keyword clashes (id_, type_).
jsonCamelOptions :: Options
jsonCamelOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "id_" -> "id"
        "type_" -> "type"
        other -> other
    }

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

data LoyaltyInfoRequest = LoyaltyInfoRequest
  { customer :: CustomerRequest,
    order :: Maybe OrderRequest
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyInfoRequest where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyInfoRequest where
  toJSON = genericToJSON jsonSnakeOptions

data CustomerRequest = CustomerRequest
  { customerId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON CustomerRequest where
  parseJSON = withObject "CustomerRequest" $ \o -> CustomerRequest <$> o .: "id"

instance ToJSON CustomerRequest where
  toJSON CustomerRequest {..} = object ["id" .= customerId]

data OrderRequest = OrderRequest
  { currency :: Text,
    amount :: Text,
    merchantId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON OrderRequest where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON OrderRequest where
  toJSON = genericToJSON jsonSnakeOptions

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

data LoyaltyInfoResponse = LoyaltyInfoResponse
  { customerId :: Text,
    programs :: Maybe [LoyaltyInfoProgram]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyInfoResponse where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyInfoResponse where
  toJSON = genericToJSON jsonCamelOptions

-- | FromJSON options for LoyaltyInfoProgram: parses Juspay snake_case,
-- including ui_label -> uiLabel and id -> id_.
walletProgramSnakeOptions :: Options
walletProgramSnakeOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "id_" -> "id"
        "uiLabel" -> "ui_label"
        other -> camelToSnakeCase other
    }

data LoyaltyInfoProgram = LoyaltyInfoProgram
  { id_ :: Text,
    advancements :: Maybe [ProgramAdvancement],
    burn :: BurnInfo,
    earn :: Maybe EarnInfo,
    membership :: ProgramMembership,
    topup :: Maybe TopupInfo,
    uiLabel :: Maybe ProgramUiLabel,
    wallet :: ProgramWallet
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyInfoProgram where
  parseJSON = genericParseJSON walletProgramSnakeOptions

instance ToJSON LoyaltyInfoProgram where
  toJSON = genericToJSON jsonCamelOptions

data CommonStatus = ACTIVE | ELIGIBLE | INELIGIBLE
  deriving stock (Show, Eq, Read, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdvancementStatus = COMPLETED | PENDING | WILL_COMPLETE
  deriving stock (Show, Eq, Read, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TopupInfo = TopupInfo
  { status :: Maybe CommonStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON TopupInfo where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON TopupInfo where
  toJSON = genericToJSON jsonCamelOptions

data ProgramAdvancement = ProgramAdvancement
  { campaignId :: Text,
    checkpoints :: [AdvancementCheckpoint],
    completion :: Maybe AdvancementCompletion,
    progress :: AdvancementProgress,
    stats :: Maybe AdvancementStats,
    timing :: Maybe AdvancementTiming
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramAdvancement where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramAdvancement where
  toJSON = genericToJSON jsonCamelOptions

data AdvancementCompletion = AdvancementCompletion
  { earn :: EarnInfo,
    status :: AdvancementStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementCompletion where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON AdvancementCompletion where
  toJSON = genericToJSON jsonCamelOptions

data AdvancementCheckpoint = AdvancementCheckpoint
  { current :: Maybe Text,
    earn :: Maybe EarnInfo,
    status :: Maybe AdvancementStatus,
    step :: Maybe Int,
    threshold :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementCheckpoint where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON AdvancementCheckpoint where
  toJSON = genericToJSON jsonCamelOptions

data CheckpointApplicable = CheckpointApplicable
  { points :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON CheckpointApplicable where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON CheckpointApplicable where
  toJSON = genericToJSON jsonCamelOptions

data AdvancementProgress = AdvancementProgress
  { completed :: Text,
    percent :: Float,
    target :: Text,
    unit :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdvancementStats = AdvancementStats
  { bestStreak :: Maybe Int,
    completions :: Maybe Int,
    currentStreak :: Maybe Int,
    totalEarned :: Maybe TotalEarned
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementStats where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON AdvancementStats where
  toJSON = genericToJSON jsonCamelOptions

newtype TotalEarned = TotalEarned
  { points :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdvancementTiming = AdvancementTiming
  { windowEndsAt :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementTiming where
  parseJSON = withObject "AdvancementTiming" $ \o -> AdvancementTiming <$> o .:? "window_ends_at"

instance ToJSON AdvancementTiming where
  toJSON AdvancementTiming {..} = object ["windowEndsAt" .= windowEndsAt]

data BurnInfo = BurnInfo
  { options :: Maybe [BurnOption]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BurnInfo where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON BurnInfo where
  toJSON = genericToJSON jsonCamelOptions

-- | FromJSON options for BurnOption: parses Juspay snake_case,
-- including id -> id_ and type -> type_.
burnOptionSnakeOptions :: Options
burnOptionSnakeOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "id_" -> "id"
        "type_" -> "type"
        other -> camelToSnakeCase other
    }

data BurnOption = BurnOption
  { applicable :: Maybe BurnApplicable,
    id_ :: Text,
    increment :: Maybe Text,
    partialAllowed :: Maybe Bool,
    status :: Maybe CommonStatus,
    type_ :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BurnOption where
  parseJSON = genericParseJSON burnOptionSnakeOptions

instance ToJSON BurnOption where
  toJSON = genericToJSON jsonCamelOptions

data BurnApplicable = BurnApplicable
  { label :: Maybe Text,
    maxPoints :: Maybe Text,
    maxValue :: Maybe Text,
    minPoints :: Maybe Text,
    rate :: Maybe BurnRate
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BurnApplicable where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON BurnApplicable where
  toJSON = genericToJSON jsonCamelOptions

data BurnRate = BurnRate
  { points :: Maybe Text,
    value :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BurnRate where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON BurnRate where
  toJSON = genericToJSON jsonCamelOptions

data EarnInfo = EarnInfo
  { applicable :: Maybe EarnApplicable,
    status :: Maybe CommonStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON EarnInfo where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON EarnInfo where
  toJSON = genericToJSON jsonCamelOptions

data EarnApplicable = EarnApplicable
  { points :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON EarnApplicable where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON EarnApplicable where
  toJSON = genericToJSON jsonCamelOptions

data ProgramMembership = ProgramMembership
  { enrolledDate :: Text,
    status :: CommonStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramMembership where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramMembership where
  toJSON = genericToJSON jsonCamelOptions

data ProgramUiLabel = ProgramUiLabel
  { description :: Maybe Text,
    title :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramUiLabel where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramUiLabel where
  toJSON = genericToJSON jsonCamelOptions

data ProgramWallet = ProgramWallet
  { availablePoints :: Maybe Text,
    expiring :: Maybe WalletExpiring,
    lifetimeEarned :: Maybe Text,
    lifetimeRedeemed :: Maybe Text,
    pendingRelease :: Maybe WalletPendingRelease,
    pockets :: Maybe [WalletPocket]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramWallet where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramWallet where
  toJSON = genericToJSON jsonCamelOptions

data WalletPocket = WalletPocket
  { availablePoints :: Maybe Text,
    label :: Maybe Text,
    lifetimeEarned :: Maybe Text,
    lifetimeRedeemed :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON WalletPocket where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON WalletPocket where
  toJSON = genericToJSON jsonCamelOptions

data WalletExpiring = WalletExpiring
  { points :: Maybe Text,
    schedule :: Maybe [WalletScheduleEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON WalletExpiring where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON WalletExpiring where
  toJSON = genericToJSON jsonCamelOptions

data WalletPendingRelease = WalletPendingRelease
  { points :: Maybe Text,
    schedule :: Maybe [WalletScheduleEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON WalletPendingRelease where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON WalletPendingRelease where
  toJSON = genericToJSON jsonCamelOptions

data WalletScheduleEntry = WalletScheduleEntry
  { date :: Maybe Text,
    points :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON WalletScheduleEntry where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON WalletScheduleEntry where
  toJSON = genericToJSON jsonCamelOptions

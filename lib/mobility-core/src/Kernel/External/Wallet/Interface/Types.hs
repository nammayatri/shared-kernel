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

jsonSnakeOptions :: Options
jsonSnakeOptions = defaultOptions {fieldLabelModifier = camelToSnakeCase}

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
    programs :: [LoyaltyInfoProgram]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyInfoResponse where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON LoyaltyInfoResponse where
  toJSON = genericToJSON jsonSnakeOptions

walletProgramOptions :: Options
walletProgramOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "id_" -> "id"
        "uiLabel" -> "ui_label"
        other -> camelToSnakeCase other
    }

data LoyaltyInfoProgram = LoyaltyInfoProgram
  { id_ :: Text,
    code :: Text,
    status :: Text,
    advancements :: [ProgramAdvancement],
    burn :: Maybe BurnInfo,
    earn :: Maybe EarnInfo,
    membership :: Maybe ProgramMembership,
    uiLabel :: Maybe ProgramUiLabel,
    wallet :: Maybe ProgramWallet
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LoyaltyInfoProgram where
  parseJSON = genericParseJSON walletProgramOptions

instance ToJSON LoyaltyInfoProgram where
  toJSON = genericToJSON walletProgramOptions

data ProgramAdvancement = ProgramAdvancement
  { checkpoints :: [AdvancementCheckpoint],
    progress :: Maybe AdvancementProgress,
    renderAs :: Maybe Text,
    stats :: Maybe AdvancementStats,
    timing :: Maybe AdvancementTiming
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramAdvancement where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramAdvancement where
  toJSON = genericToJSON jsonSnakeOptions

data AdvancementCheckpoint = AdvancementCheckpoint
  { applicable :: Maybe CheckpointApplicable,
    completedAt :: Maybe Text,
    label :: Maybe Text,
    value :: Maybe Text,
    ineligibleReason :: Maybe [IneligibleReason]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementCheckpoint where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON AdvancementCheckpoint where
  toJSON = genericToJSON jsonSnakeOptions

data CheckpointApplicable = CheckpointApplicable
  { points :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdvancementProgress = AdvancementProgress
  { current :: Text,
    percent :: Text,
    target :: Text,
    unit :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdvancementStats = AdvancementStats
  { streakBeforeBreak :: Maybe Text,
    streakBest :: Maybe Text,
    streakCurrent :: Maybe Text,
    timesCompleted :: Maybe Text,
    totalEarned :: Maybe TotalEarned
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementStats where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON AdvancementStats where
  toJSON = genericToJSON jsonSnakeOptions

newtype TotalEarned = TotalEarned
  { points :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AdvancementTiming = AdvancementTiming
  { actionBy :: Maybe Text,
    actionDone :: Maybe Bool,
    endsAt :: Maybe Text,
    isBroken :: Maybe Bool,
    resetsAt :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON AdvancementTiming where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON AdvancementTiming where
  toJSON = genericToJSON jsonSnakeOptions

data BurnInfo = BurnInfo
  { options :: [BurnOption]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

burnOptionOptions :: Options
burnOptionOptions =
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
    ineligibleReasons :: Maybe [IneligibleReason],
    partialAllowed :: Maybe Bool,
    status :: Maybe Text,
    type_ :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BurnOption where
  parseJSON = genericParseJSON burnOptionOptions

instance ToJSON BurnOption where
  toJSON = genericToJSON burnOptionOptions

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
  toJSON = genericToJSON jsonSnakeOptions

data BurnRate = BurnRate
  { points :: Text,
    value :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data EarnInfo = EarnInfo
  { applicable :: Maybe EarnApplicable,
    ineligibleReasons :: Maybe [IneligibleReason],
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON EarnInfo where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON EarnInfo where
  toJSON = genericToJSON jsonSnakeOptions

data EarnApplicable = EarnApplicable
  { label :: Maybe Text,
    points :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data IneligibleReason = IneligibleReason
  { code :: Maybe Text,
    message :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

ineligibleActionOptions :: Options
ineligibleActionOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "type_" -> "type"
        other -> other
    }

data IneligibleAction = IneligibleAction
  { cta :: Maybe Text,
    type_ :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON IneligibleAction where
  parseJSON = genericParseJSON ineligibleActionOptions

instance ToJSON IneligibleAction where
  toJSON = genericToJSON ineligibleActionOptions

data ProgramMembership = ProgramMembership
  { enrollOn :: Maybe Text,
    enrolledDate :: Maybe Text,
    ineligibleReason :: Maybe [IneligibleReason],
    status :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramMembership where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramMembership where
  toJSON = genericToJSON jsonSnakeOptions

data ProgramUiLabel = ProgramUiLabel
  { asset :: Maybe ProgramAsset,
    customLogo :: Maybe Text,
    description :: Maybe Text,
    title :: Maybe Text,
    tnc :: Maybe Text,
    tncUrl :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramUiLabel where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramUiLabel where
  toJSON = genericToJSON jsonSnakeOptions

data ProgramAsset = ProgramAsset
  { code :: Maybe Text,
    customIcon :: Maybe Text,
    name :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramAsset where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramAsset where
  toJSON = genericToJSON jsonSnakeOptions

data ProgramWallet = ProgramWallet
  { availablePoints :: Maybe Text,
    expiring :: Maybe WalletExpiring,
    lifetimeEarned :: Maybe Text,
    lifetimeRedeemed :: Maybe Text,
    pendingRelease :: Maybe WalletPendingRelease
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ProgramWallet where
  parseJSON = genericParseJSON jsonSnakeOptions

instance ToJSON ProgramWallet where
  toJSON = genericToJSON jsonSnakeOptions

data WalletExpiring = WalletExpiring
  { points :: Maybe Text,
    schedule :: [WalletScheduleEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletPendingRelease = WalletPendingRelease
  { points :: Maybe Text,
    schedule :: [WalletScheduleEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data WalletScheduleEntry = WalletScheduleEntry
  { date :: Maybe Text,
    points :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

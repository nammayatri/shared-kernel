{-# LANGUAGE DerivingStrategies #-}

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
    module Reexport,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Types (typeMismatch)
import Data.OpenApi (NamedSchema (..), declareNamedSchema)
import qualified Data.OpenApi as OpenApi
import qualified Kernel.External.Wallet.Juspay.Config as Juspay
import Kernel.External.Wallet.Types as Reexport
import Kernel.Prelude
import Kernel.Types.Common

data WalletServiceConfig = JuspayWalletConfig Juspay.JuspayWalletConfig
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateWalletReq = CreateWalletReq
  { operationId :: Text,
    createCashAccount :: Bool,
    createPointsAccount :: Bool,
    customerId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype CreateWalletResp = CreateWalletResp
  { rawResponse :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreateWalletResp where
  parseJSON = pure . CreateWalletResp

instance ToJSON CreateWalletResp where
  toJSON = rawResponse

instance ToSchema CreateWalletResp where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)

data WalletPostingType = Grant | Redeem | Refund
  deriving (Show, Eq, Ord, Generic)

data WalletPostingReq = WalletPostingReq
  { customerId :: Text,
    postingType :: WalletPostingType,
    operationId :: Text,
    pointsAmount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype WalletPostingResp = WalletPostingResp
  { rawResponse :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WalletPostingResp where
  parseJSON = pure . WalletPostingResp

instance ToJSON WalletPostingResp where
  toJSON = rawResponse

instance ToSchema WalletPostingResp where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)

instance ToJSON WalletPostingType where
  toJSON = String . walletPostingTypeToText

instance FromJSON WalletPostingType where
  parseJSON (String t) = maybe (typeMismatch "WalletPostingType" (String t)) pure (walletPostingTypeFromText t)
  parseJSON invalid = typeMismatch "WalletPostingType" invalid

instance ToSchema WalletPostingType where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just "WalletPostingType")
        ( mempty
            { OpenApi._schemaType = Just OpenApi.OpenApiString,
              OpenApi._schemaEnum = Just (String . walletPostingTypeToText <$> [Redeem, Refund, Grant])
            }
        )

walletPostingTypeToText :: WalletPostingType -> Text
walletPostingTypeToText Redeem = "REDEEM"
walletPostingTypeToText Refund = "REFUND"
walletPostingTypeToText Grant = "GRANT"

walletPostingTypeFromText :: Text -> Maybe WalletPostingType
walletPostingTypeFromText "REDEEM" = Just Redeem
walletPostingTypeFromText "REFUND" = Just Refund
walletPostingTypeFromText "GRANT" = Just Grant
walletPostingTypeFromText _ = Nothing

data WalletReversalReq = WalletReversalReq
  { customerId :: Text,
    operationId :: Text,
    txnOperationId :: Text,
    amount :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype WalletReversalResp = WalletReversalResp
  { rawResponse :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WalletReversalResp where
  parseJSON = pure . WalletReversalResp

instance ToJSON WalletReversalResp where
  toJSON = rawResponse

instance ToSchema WalletReversalResp where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)

data WalletBalanceReq = WalletBalanceReq
  { customerId :: Text,
    requireHistory :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype WalletBalanceResp = WalletBalanceResp
  { rawResponse :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WalletBalanceResp where
  parseJSON = pure . WalletBalanceResp

instance ToJSON WalletBalanceResp where
  toJSON = rawResponse

instance ToSchema WalletBalanceResp where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)

newtype WalletVerifyTxnReq = WalletVerifyTxnReq
  { operationId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype WalletVerifyTxnResp = WalletVerifyTxnResp
  { rawResponse :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WalletVerifyTxnResp where
  parseJSON = pure . WalletVerifyTxnResp

instance ToJSON WalletVerifyTxnResp where
  toJSON = rawResponse

instance ToSchema WalletVerifyTxnResp where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)

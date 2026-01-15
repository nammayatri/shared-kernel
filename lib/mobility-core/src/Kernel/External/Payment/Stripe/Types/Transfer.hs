{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payment.Stripe.Types.Transfer where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Price (Currency)
import Kernel.Utils.JSON
import Web.FormUrlEncoded
import Web.HttpApiData (ToHttpApiData (..))

newtype TransferId = TransferId {getTransferId :: Text}
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema)

data TransferReq = TransferReq
  { amount :: Int,
    currency :: Currency,
    destination :: AccountId,
    metadata :: Maybe Metadata,
    description :: Maybe Text
  }
  deriving stock (Show, Generic)

instance ToForm TransferReq where
  toForm TransferReq {..} =
    Form $
      HM.fromList $
        catMaybes
          [ Just . ("amount",) . pure $ toQueryParam amount,
            Just . ("currency",) . pure $ toQueryParam currency,
            Just . ("destination",) . pure $ toQueryParam destination,
            ("description",) . pure . toQueryParam <$> description
          ]

-- TODO webhook transfer.created transfer.failed transfer.reversed transfer.updated
data TransferObject = TransferObject
  { id :: TransferId,
    _object :: Text,
    amount :: Int,
    created :: UTCTime,
    currency :: Currency,
    destination :: AccountId,
    status :: TransferStatus
  }
  deriving stock (Show, Generic)

instance FromJSON TransferObject where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TransferObject where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data TransferStatus
  = TRANSFER_PENDING
  | TRANSFER_IN_TRANSIT
  | TRANSFER_CANCELED
  | TRANSFER_FAILED
  | TRANSFER_SUCCEEDED
  | TRANSFER_REVERSED
  deriving stock (Show, Eq, Ord, Generic, Read)
  deriving anyclass (ToSchema)

transferStatusJsonOptions :: Options
transferStatusJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "TRANSFER_PENDING" -> "pending"
        "TRANSFER_IN_TRANSIT" -> "in_transit"
        "TRANSFER_CANCELED" -> "canceled"
        "TRANSFER_FAILED" -> "failed"
        "TRANSFER_SUCCEEDED" -> "succeeded"
        "TRANSFER_REVERSED" -> "reversed"
        x -> x
    }

instance FromJSON TransferStatus where
  parseJSON = genericParseJSON transferStatusJsonOptions

instance ToJSON TransferStatus where
  toJSON = genericToJSON transferStatusJsonOptions

derivePersistField "TransferStatus"

$(mkBeamInstancesForEnum ''TransferStatus)

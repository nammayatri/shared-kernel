{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payout.Stripe.Types.Transfer where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Utils.JSON
import Web.FormUrlEncoded
import Web.HttpApiData (ToHttpApiData (..))

newtype TransferId = TransferId {getTransferId :: Text}
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema)

data TransferReq = TransferReq
  { amount :: Int,
    currency :: Text,
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

-- TODO webhook transfer.created, transfer.reversed, transfer.updated
-- Currently transfer api would throw error instead of transfer object in case of failure, so there is no status field
data TransferObject = TransferObject
  { id :: TransferId,
    _object :: Text,
    amount :: Int,
    created :: POSIXTime,
    currency :: Text,
    destination :: AccountId
  }
  deriving stock (Show, Generic)

instance FromJSON TransferObject where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TransferObject where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

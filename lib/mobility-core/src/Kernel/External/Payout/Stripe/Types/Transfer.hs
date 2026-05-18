{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Payout.Stripe.Types.Transfer where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.External.Payment.Stripe.Types.Common (AccountId)
import Kernel.External.Payout.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Utils.JSON
import qualified Kernel.Utils.Schema as S
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
            ("description",) . pure . toQueryParam <$> description,
            ("metadata[order_id]",) . pure . toQueryParam <$> (metadata >>= (.order_id)),
            ("metadata[customer_id]",) . pure . toQueryParam <$> (metadata >>= (.customer_id)),
            ("metadata[order_type]",) . pure . toQueryParam <$> (metadata >>= (.order_type))
          ]

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

instance ToSchema TransferObject where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

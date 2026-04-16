{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payout.Stripe.Types.Common where

import Data.Aeson
import Kernel.Prelude

-- up to 50 custom defined fields
data Metadata = Metadata
  { order_id :: Maybe Text,
    customer_id :: Maybe Text,
    order_type :: Maybe Text
    -- payout_request_id :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

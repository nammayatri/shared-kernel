{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kernel.External.SMS.PinbixSms.Types where

import Data.Aeson
import EulerHS.Prelude

data PinbixSmsResponse = PinbixSmsResponse
  { status :: Text,
    statusCode :: Maybe Text,
    reason :: Maybe Text,
    transactionId :: Maybe Text,
    mobile :: Maybe Text,
    invalidMobile :: Maybe Text,
    msgId :: Maybe Text,
    requestTime :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

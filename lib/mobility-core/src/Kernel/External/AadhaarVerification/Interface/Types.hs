{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.AadhaarVerification.Interface.Types
  ( module Kernel.External.AadhaarVerification.Interface.Types,
  )
where

import qualified Kernel.External.AadhaarVerification.Gridline.Config as Gridline
import Kernel.Prelude

newtype AadhaarVerificationServiceConfig = GridlineConfig Gridline.GridlineCfg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AadhaarVerificationResp = AadhaarVerificationResp
  { statusCode :: Text,
    message :: Text,
    transactionId :: Maybe Text,
    requestId :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

data AadhaarOtpReq = AadhaarOtpReq
  { aadhaarNumber :: Text,
    consent :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

data AadhaarOtpVerifyReq = AadhaarOtpVerifyReq
  { otp :: Int,
    shareCode :: Text,
    transactionId :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

data AadhaarOtpVerifyRes = AadhaarOtpVerifyRes
  { transactionId :: Text,
    message :: Text,
    code :: Text,
    name :: Text,
    gender :: Text,
    date_of_birth :: Text,
    share_code :: Text,
    image :: Text,
    request_id :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Juspay.Config where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Time

data JuspayCfg = JuspayCfg
  { apiKey :: EncryptedField 'AsEncrypted Text,
    returnUrl :: BaseUrl,
    url :: BaseUrl,
    merchantId :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    serviceMode :: Maybe ServiceMode,
    gatewayReferenceId :: Maybe Text,
    pseudoClientId :: Maybe Text, --- it is passed in response to for handling multiple merchants in same payment page
    isSplitEnabled :: Maybe Bool,
    isRefundSplitEnabled :: Maybe Bool,
    isPercentageSplit :: Maybe Bool,
    paymentOrderValidity :: Maybe Seconds,
    offerSKUConfig :: Maybe Text,
    autoRefundConflictThresholdMinutes :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ServiceMode = AA | NonAA
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

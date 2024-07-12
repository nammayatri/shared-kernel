{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Tokenize.Interface.Types where

import qualified Kernel.External.Tokenize.Gullak.Types as GUKTypes
import qualified Kernel.External.Tokenize.HyperVerge.Types as HVTypes
import Kernel.External.Tokenize.JourneyMonitoring.Types as JMTypes
import Kernel.Prelude

data TokenizationServiceConfig = HyperVergeTokenizationServiceConfig HVTypes.HyperVergeTokenizeConfig | JourneyMonitoringTokenizationServiceConfig JMTypes.JourneyMonitoringTokenizeConfig | GullakTokenizationServiceConfig GUKTypes.GullakConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype TokenizationReq = TokenizationReq
  { expiry :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TokenizationResp = TokenizationResp
  { token :: Text,
    expiresAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OnboardingAndLoginRes = OnboardingAndLoginRes
  { status :: Text,
    loginToken :: LoginToken
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OnboardingReq = OnboardingReq
  { merchantUserId :: Text,
    mobile :: Text,
    email :: Maybe Text,
    dob :: Maybe Text,
    pan :: Maybe Text,
    name :: Maybe Text,
    isKycVerified :: Maybe Bool,
    address :: Maybe Address,
    languageDetails :: Maybe LanguageDetails,
    locationDetails :: Maybe LocationDetails
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LanguageDetails = LanguageDetails
  { defaultLanguage :: Text,
    languagesKnown :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LocationDetails = LocationDetails
  { sparseCoordinates :: [Double],
    state :: Text,
    city :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype LoginReq = LoginReq
  { merchantUserId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LoginToken = LoginToken
  { accessToken :: Text,
    expiryDate :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Address = Address
  { name :: Text,
    mobileNo :: Text,
    state :: Text,
    city :: Text,
    pincode :: Text,
    address :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

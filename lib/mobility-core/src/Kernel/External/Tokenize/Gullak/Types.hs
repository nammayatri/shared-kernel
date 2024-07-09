{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Tokenize.Gullak.Types where

import Kernel.External.Encryption
import Kernel.Prelude hiding (error)

data GullakConfig = GullakConfig
  { url :: BaseUrl,
    udf :: Maybe Text,
    merchantId :: Text,
    apiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

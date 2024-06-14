{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.HyperVerge.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude hiding (error)

data HyperVergeVerificationCfg = HyperVergeVerificationCfg
  { url :: BaseUrl,
    appId :: Text,
    appKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype HyperVergeSdkVerificationReq = HyperVergeSdkVerificationReq
  { transactionId :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- HVTODO: Ask Appkey is unsecured why you can send anything and it works?

newtype Metadata = Metadata
  { requestId :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Sources = Sources
  { source :: Maybe Text,
    subSource :: Maybe Text,
    values :: Maybe Object
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data SelfieFlow = SelfieFlow
  { selfieURL :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AadhaarFlow = AadhaarFlow
  { idNumber :: Maybe Text,
    fullName :: Maybe Text,
    dob :: Maybe Text,
    address :: Maybe Text,
    city :: Maybe Text,
    pincode :: Maybe Text,
    aadhaarFrontURL :: Text,
    aadhaarBackURL :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PanFlow = PanFlow
  { pan :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    panURL :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UserDetails = HVSelfieFlow SelfieFlow | HVAadhaarFlow AadhaarFlow | HVPanFlow PanFlow
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON UserDetails where
  parseJSON = withObject "UserDetails" $ \v ->
    (HVAadhaarFlow <$> parseJSON (Object v))
      <|> (HVPanFlow <$> parseJSON (Object v))
      <|> (HVSelfieFlow <$> parseJSON (Object v))

data HVResult = HVResult
  { flags :: Maybe [Sources],
    userDetails :: Maybe UserDetails,
    status :: Maybe Text,
    transactionId :: Maybe Text,
    failureReason :: Maybe Text,
    error :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data HyperVergeSdkVerificationRes = HyperVergeSdkVerificationRes
  { status :: Maybe Text,
    statusCode :: Maybe Int,
    metadata :: Maybe Metadata,
    result :: Maybe HVResult
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

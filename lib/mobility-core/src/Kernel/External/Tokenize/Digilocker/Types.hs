{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Tokenize.Digilocker.Types where

import Data.Aeson (Value (..), withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Kernel.External.Encryption
import Kernel.Prelude hiding (error)
import Web.FormUrlEncoded (ToForm)

data DigilockerTokenizeConfig = DigilockerTokenizeConfig
  { url :: BaseUrl,
    clientId :: Text,
    clientSecret :: EncryptedField 'AsEncrypted Text,
    redirectUri :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DigilockerTokenizeRequest = DigilockerTokenizeRequest
  { grant_type :: Text,
    code :: Text,
    client_id :: Text,
    client_secret :: Text,
    redirect_uri :: Text,
    code_verifier :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm)

data DigilockerTokenizeResponse = DigilockerTokenizeResponse
  { access_token :: Text,
    token_type :: Maybe Text,
    expires_in :: Maybe Int,
    scope :: Maybe Text,
    consent_valid_till :: Maybe Int,
    digilockerid :: Maybe Text,
    name :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    eaadhaar :: Maybe Text,
    reference_key :: Maybe Text,
    mobile :: Maybe Text,
    refresh_token :: Maybe Text,
    new_account :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON DigilockerTokenizeResponse where
  parseJSON = withObject "DigilockerTokenizeResponse" $ \obj -> do
    access_token <- obj .: "access_token"
    token_type <- obj .:? "token_type"
    expires_in <- obj .:? "expires_in"
    scope <- obj .:? "scope"
    consent_valid_till <- parseConsentValidTill =<< obj .:? "consent_valid_till"
    digilockerid <- obj .:? "digilockerid"
    name <- obj .:? "name"
    dob <- obj .:? "dob"
    gender <- obj .:? "gender"
    eaadhaar <- obj .:? "eaadhaar"
    reference_key <- obj .:? "reference_key"
    mobile <- obj .:? "mobile"
    refresh_token <- obj .:? "refresh_token"
    new_account <- obj .:? "new_account"
    pure DigilockerTokenizeResponse {..}
    where
      parseConsentValidTill :: Maybe Value -> Parser (Maybe Int)
      parseConsentValidTill Nothing = pure Nothing
      parseConsentValidTill (Just (Number n)) = pure $ Just (floor n)
      parseConsentValidTill (Just (String s)) = case readMaybe (T.unpack s) of
        Just i -> pure $ Just i
        Nothing -> fail $ "Could not parse consent_valid_till as Int: " <> T.unpack s
      parseConsentValidTill (Just v) = fail $ "Expected Int or String for consent_valid_till, got: " <> show v

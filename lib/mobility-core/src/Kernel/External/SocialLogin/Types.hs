{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.External.SocialLogin.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Utils.Dhall (FromDhall)
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData (toQueryParam)

data GoogleSignInConfig = GoogleSignInConfig
  { clientIds :: [Text]
  }
  deriving (Show, Eq, Generic, FromDhall)

data AppleSignInConfig = AppleSignInConfig
  { clientIds :: [Text],
    teamId :: Text,
    keyId :: Text,
    encryptedPrivateKey :: Text
  }
  deriving (Show, Eq, Generic, FromDhall)

data SocialLoginConfig = SocialLoginConfig
  { google :: Maybe GoogleSignInConfig,
    apple :: [AppleSignInConfig],
    jwksCacheExpiry :: Seconds,
    clockSkew :: Seconds
  }
  deriving (Show, Eq, Generic, FromDhall)

data VerifiedSocialIdentity = VerifiedSocialIdentity
  { issuer :: Text,
    subject :: Text,
    audience :: Text,
    email :: Maybe Text,
    emailVerified :: Bool,
    isPrivateEmail :: Bool,
    name :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Jwk = Jwk
  { kty :: Text,
    kid :: Text,
    n :: Maybe Text,
    e :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype JwkSet = JwkSet
  { keys :: [Jwk]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppleTokenReq = AppleTokenReq
  { clientId :: Text,
    clientSecret :: Text,
    code :: Text
  }
  deriving (Show, Eq, Generic)

instance ToForm AppleTokenReq where
  toForm AppleTokenReq {..} =
    [ ("client_id", toQueryParam clientId),
      ("client_secret", toQueryParam clientSecret),
      ("code", toQueryParam code),
      ("grant_type", toQueryParam ("authorization_code" :: Text))
    ]

data AppleTokenResp = AppleTokenResp
  { refresh_token :: Maybe Text,
    id_token :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON AppleTokenResp where
  toJSON resp =
    object
      [ "refresh_token" .= (("redacted" :: Text) <$ resp.refresh_token),
        "id_token" .= (("redacted" :: Text) <$ resp.id_token)
      ]

data AppleRevokeReq = AppleRevokeReq
  { clientId :: Text,
    clientSecret :: Text,
    token :: Text
  }
  deriving (Show, Eq, Generic)

instance ToForm AppleRevokeReq where
  toForm AppleRevokeReq {..} =
    [ ("client_id", toQueryParam clientId),
      ("client_secret", toQueryParam clientSecret),
      ("token", toQueryParam token),
      ("token_type_hint", toQueryParam ("refresh_token" :: Text))
    ]

data AppleRevokeResp = AppleRevokeResp
  deriving (Show, Eq, Generic)

instance ToJSON AppleRevokeResp where
  toJSON _ = Null

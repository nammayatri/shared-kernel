{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.JWT where

import Control.Applicative
import qualified Data.Aeson as J
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import EulerHS.Prelude hiding (exp, fromRight, try)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Web.JWT

-- | Google cloud service account json file format
-- it contains key id, private key and other data needed to get JWT
-- https://cloud.google.com/compute/docs/access/service-accounts
data ServiceAccount = ServiceAccount
  { saType :: !T.Text,
    saProjectId :: !T.Text,
    saPrivateKeyId :: !T.Text,
    saPrivateKey :: !String,
    saClientEmail :: !T.Text,
    saClientId :: !T.Text,
    saAuthUri :: !T.Text,
    saTokenUri :: !T.Text,
    saAuthProviderX509CertUrl :: !T.Text,
    saClientX509CertUrl :: !T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''ServiceAccount)

-- | JWT body format, is is used for retrieving JWT token
data JWTBody = JWTBody
  { jwtAssertion :: !T.Text,
    jwtGrantType :: !T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''JWTBody)

-- | JWT token returned from token url
data JWToken = JWToken
  { jwtAccessToken :: !T.Text,
    jwtExpiresIn :: Integer,
    jwtTokenType :: !T.Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''JWToken)

-- | Prepare claims and assertion needed for getting JWT
-- It is possible to add user-defined claims using the additionalClaims parameter
-- Returns a pair of claims and assertion of these claims
createJWT :: ServiceAccount -> [(Text, Value)] -> IO (Either String (JWTClaimsSet, Text))
createJWT sa additionalClaims = do
  let iss = stringOrURI . saClientEmail $ sa
  let aud = Left <$> (stringOrURI . saTokenUri $ sa)
  let unregisteredClaims = ClaimsMap $ Map.fromList additionalClaims
  let jwtHeader =
        JOSEHeader
          { typ = Just "JWT",
            cty = Nothing,
            alg = Just RS256,
            kid = Just $ saPrivateKeyId sa -- key id from sa json file
          }
  let mkey = readRsaSecret . C8.pack $ saPrivateKey sa
  case mkey of
    Nothing -> pure $ Left "Bad RSA key!"
    Just pkey -> do
      let key = RSAPrivateKey pkey
      iat <- numericDate <$> getPOSIXTime
      exp <- numericDate . (+ 3600) <$> getPOSIXTime
      let searchRequest =
            mempty
              { exp = exp, -- Expired at
                iat = iat, -- Issued at
                iss = iss, -- Issuer (client email)
                aud = aud, -- Audience (endpoints where JWT will be used)
                unregisteredClaims = unregisteredClaims -- additional claims
              }
      pure $ Right (searchRequest, encodeSigned key jwtHeader searchRequest)

-- | Prepare a request to the token URL
jwtRequest :: T.Text -> BL.ByteString -> IO Request
jwtRequest tokenUri body = do
  req <- parseRequest $ T.unpack tokenUri
  pure $
    req {method = "POST", requestHeaders = [(hContentType, "application/json")], requestBody = RequestBodyLBS body}

-- | Geto or refresh JWT token
-- Note at the moment it is used with FCM service so scope is hardcoded
doRefreshToken :: ServiceAccount -> IO (Either String JWToken)
doRefreshToken sa = do
  jwtPair <- createJWT sa [("scope", String "https://www.googleapis.com/auth/firebase.messaging")]
  case jwtPair of
    Left err -> pure $ Left err
    Right (claimPairs, assertion) -> do
      let issuedAt = iat claimPairs
      manager <- newManager tlsManagerSettings
      let body =
            JWTBody
              { jwtAssertion = assertion,
                jwtGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"
              }
      req <- jwtRequest (saTokenUri sa) (J.encode body)
      res <- httpLbs req manager
      let rBody = J.eitherDecode $ responseBody res
      case rBody of
        Left err -> pure $ Left err
        Right respBody@JWToken {..} -> do
          let expiry = getExpiry issuedAt jwtExpiresIn
          pure $
            Right
              respBody
                { jwtExpiresIn = expiry
                }

-- | Get token expiration date
getExpiry :: Maybe NumericDate -> Integer -> Integer
getExpiry Nothing expiresIn = expiresIn
getExpiry (Just d) expiresIn =
  expiresIn + round (nominalDiffTimeToSeconds $ secondsSinceEpoch d)

-- | JWT token validation status
data JWTValidity
  = JWTValid Integer -- valid and expires in X seconds
  | JWTInvalid -- invalid (bad signautre)
  | JWTExpired Integer -- expired X seconds ago
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''JWTValidity)

-- | Check token validity
isValid :: JWToken -> IO JWTValidity
isValid token = do
  let expiry = jwtExpiresIn token
  curInt <- round <$> getPOSIXTime
  -- check a signature here, not sure it is possible,
  -- for this we'd need to get a "public" key which is stored in google
  -- PS we can keep claims in options, this will allow us
  -- recreating the token and verifying it
  let valid = True
  let expired = curInt > expiry
  let diff = abs $ curInt - expiry
  pure $ case (expired, valid) of
    (True, _) -> JWTExpired diff
    (_, True) -> JWTValid diff
    _ -> JWTInvalid

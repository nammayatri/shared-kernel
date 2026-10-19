{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SocialLogin.API
  ( JwksAPI,
    jwksAPI,
    AppleTokenAPI,
    appleTokenAPI,
    AppleRevokeAPI,
    appleRevokeAPI,
    AppleEmptyResponse,
  )
where

import qualified Data.List.NonEmpty as NE
import Kernel.External.SocialLogin.Types
import Kernel.Prelude
import qualified Network.HTTP.Media as M
import Servant

data AppleEmptyResponse deriving (Typeable)

instance Accept AppleEmptyResponse where
  contentTypes _ = NE.fromList ["application" M.// "json", "*" M.// "*"]

instance MimeUnrender AppleEmptyResponse AppleRevokeResp where
  mimeUnrender _ _ = Right AppleRevokeResp

type JwksAPI = Get '[JSON] JwkSet

jwksAPI :: Proxy JwksAPI
jwksAPI = Proxy

type AppleTokenAPI =
  "auth" :> "token"
    :> ReqBody '[FormUrlEncoded] AppleTokenReq
    :> Post '[JSON] AppleTokenResp

appleTokenAPI :: Proxy AppleTokenAPI
appleTokenAPI = Proxy

type AppleRevokeAPI =
  "auth" :> "revoke"
    :> ReqBody '[FormUrlEncoded] AppleRevokeReq
    :> Post '[AppleEmptyResponse] AppleRevokeResp

appleRevokeAPI :: Proxy AppleRevokeAPI
appleRevokeAPI = Proxy

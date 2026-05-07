{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.MasterCloudForward.Types where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

-- | Envelope for an outbound HTTP request that needs to be re-issued from the
-- AWS-side egress IP. Body is base64-encoded so that arbitrary binary payloads
-- can be ferried through JSON safely.
data ForwardRequest = ForwardRequest
  { -- | HTTP verb, e.g. "GET", "POST"
    method :: Text,
    -- | Fully-qualified URL: scheme + host + port + path + query string
    url :: Text,
    -- | Request headers (textual)
    headers :: [(Text, Text)],
    -- | Base64-encoded raw request body bytes
    body :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Envelope for the response returned by the forwarder, mirroring the
-- upstream HTTP response.
data ForwardResponse = ForwardResponse
  { status :: Int,
    headers :: [(Text, Text)],
    -- | Base64-encoded response body bytes
    body :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ForwardError
  = ForwardAllowlistDenied Text
  | ForwardInvalidUrl Text
  | ForwardUpstreamFailure Text
  | ForwardAuthFailure
  deriving (Eq, Generic, Show, IsBecknAPIError, FromJSON, ToJSON)

instanceExceptionWithParent 'HTTPException ''ForwardError

instance IsBaseError ForwardError where
  toMessage = \case
    ForwardAllowlistDenied host -> Just $ "Host not in egress allowlist: " <> host
    ForwardInvalidUrl url' -> Just $ "Invalid forwarding URL: " <> url'
    ForwardUpstreamFailure msg -> Just $ "Upstream forwarder failure: " <> msg
    ForwardAuthFailure -> Just "Forwarder authentication failed."

instance IsHTTPError ForwardError where
  toErrorCode = \case
    ForwardAllowlistDenied _ -> "FORWARDER_ALLOWLIST_DENIED"
    ForwardInvalidUrl _ -> "FORWARDER_INVALID_URL"
    ForwardUpstreamFailure _ -> "FORWARDER_UPSTREAM_FAILURE"
    ForwardAuthFailure -> "FORWARDER_AUTH_FAILURE"

  toHttpCode = \case
    ForwardAllowlistDenied _ -> E403
    ForwardInvalidUrl _ -> E400
    ForwardUpstreamFailure _ -> E500
    ForwardAuthFailure -> E401

instance IsAPIError ForwardError

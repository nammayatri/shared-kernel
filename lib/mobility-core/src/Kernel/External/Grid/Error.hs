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

module Kernel.External.Grid.Error where

import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))
import Prelude

-- | Typed Grid (LiteLLM proxy) failures. Other statuses surface as 'ExternalAPICallError'.
data GridError
  = -- | 401 / 403: bad, expired or unauthorised key. A config problem; never retry.
    GridUnauthorized
  | -- | 400 / 422: the proxy rejected our request. A bug on our side; never retry.
    GridBadRequest
  | -- | 429: rate or budget limit on the key or the upstream model. Back off.
    GridRateLimited
  | -- | 408 / 502 / 503 / 504: timeout or upstream unavailable. Safe to retry once with backoff.
    GridUnavailable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GridError

instance FromResponse GridError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just GridBadRequest
    401 -> Just GridUnauthorized
    403 -> Just GridUnauthorized
    408 -> Just GridUnavailable
    422 -> Just GridBadRequest
    429 -> Just GridRateLimited
    502 -> Just GridUnavailable
    503 -> Just GridUnavailable
    504 -> Just GridUnavailable
    _ -> Nothing

instance IsBaseError GridError where
  toMessage = \case
    GridUnauthorized -> Just "Grid rejected the API key."
    GridBadRequest -> Just "Grid rejected the decision request."
    GridRateLimited -> Just "Grid rate or budget limit reached."
    GridUnavailable -> Just "Grid is temporarily unavailable."

instance IsHTTPError GridError where
  toErrorCode = \case
    GridUnauthorized -> "GRID_UNAUTHORIZED"
    GridBadRequest -> "GRID_BAD_REQUEST"
    GridRateLimited -> "GRID_RATE_LIMITED"
    GridUnavailable -> "GRID_UNAVAILABLE"

  toHttpCode = \case
    GridUnauthorized -> E500
    GridBadRequest -> E500
    GridRateLimited -> E503
    GridUnavailable -> E503

instance IsAPIError GridError

-- | Whether the caller may retry (once, with backoff).
isGridRetryable :: GridError -> Bool
isGridRetryable = \case
  GridUnavailable -> True
  GridRateLimited -> True
  _ -> False

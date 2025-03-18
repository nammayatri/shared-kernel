{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.SafetyPortal.Error where

import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Kernel.Utils.Error.TH (mkOpenAPIError)
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))
import Prelude

data SafetyPortalError
  = SafetyPortalBadRequest
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SafetyPortalError

instance FromResponse SafetyPortalError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just SafetyPortalBadRequest
    _ -> Nothing

instance IsBaseError SafetyPortalError where
  toMessage = \case
    SafetyPortalBadRequest -> Just "Bad request. Please check for the input."

instance IsHTTPError SafetyPortalError where
  toErrorCode = \case
    SafetyPortalBadRequest -> "BAD_REQUEST"

  toHttpCode = \case
    SafetyPortalBadRequest -> E400

instance IsAPIError SafetyPortalError

mkOpenAPIError ''SafetyPortalError

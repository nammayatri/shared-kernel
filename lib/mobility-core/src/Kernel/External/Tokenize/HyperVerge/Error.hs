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

module Kernel.External.Tokenize.HyperVerge.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data HyperVergeError
  = HVUnauthorizedError
  | HVBadRequestError Text
  | HVError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HyperVergeError

instance IsBaseError HyperVergeError where
  toMessage = \case
    HVUnauthorizedError -> Just "Invalid Credentaials, Please provide valida appId and appKey."
    HVBadRequestError msg -> Just $ "Bad Request with message: " <> msg
    HVError msg -> Just $ "HyperVerge Error with message: " <> msg

instance IsHTTPError HyperVergeError where
  toErrorCode = \case
    HVUnauthorizedError -> "HV_UNAUTHORIZED"
    HVBadRequestError _ -> "HV_BAD_REQUEST"
    HVError _ -> "HV_ERROR"

  toHttpCode = \case
    HVUnauthorizedError -> E401
    HVBadRequestError _ -> E400
    HVError _ -> E400

instance IsAPIError HyperVergeError

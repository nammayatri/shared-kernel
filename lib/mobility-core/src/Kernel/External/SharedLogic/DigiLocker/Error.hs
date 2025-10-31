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

module Kernel.External.SharedLogic.DigiLocker.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data DigiLockerError
  = DLError Text
  | DLUnauthorizedError
  | DLBadRequestError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DigiLockerError

instance IsBaseError DigiLockerError where
  toMessage = \case
    DLError msg -> Just $ "DigiLocker Error: " <> msg
    DLUnauthorizedError -> Just "DigiLocker token expired or has been revoked by DigiLocker user."
    DLBadRequestError msg -> Just $ "DigiLocker Bad Request: " <> msg

instance IsHTTPError DigiLockerError where
  toErrorCode = \case
    DLError _ -> "DL_ERROR"
    DLUnauthorizedError -> "DL_UNAUTHORIZED"
    DLBadRequestError _ -> "DL_BAD_REQUEST"

  toHttpCode = \case
    DLUnauthorizedError -> E401
    DLBadRequestError _ -> E400
    DLError _ -> E400

instance IsAPIError DigiLockerError

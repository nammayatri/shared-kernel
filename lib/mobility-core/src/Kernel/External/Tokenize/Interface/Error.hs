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

module Kernel.External.Tokenize.Interface.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data TokenizationError
  = TokenNotFound Text
  | ExpiryNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TokenizationError

instance IsBaseError TokenizationError where
  toMessage = \case
    TokenNotFound svcName -> Just $ "Token Not found in Tokenization response of service provider" <> svcName
    ExpiryNotFound svcName -> Just $ "Expiry Not found in Tokenization response of service provider" <> svcName

instance IsHTTPError TokenizationError where
  toErrorCode = \case
    TokenNotFound _ -> "TOKEN_NOT_FOUND"
    ExpiryNotFound _ -> "EXPIRY_NOT_FOUND"

  toHttpCode = \case
    TokenNotFound _ -> E500
    ExpiryNotFound _ -> E500

instance IsAPIError TokenizationError

-- mkOpenAPIError ''TokenizationError -- E500

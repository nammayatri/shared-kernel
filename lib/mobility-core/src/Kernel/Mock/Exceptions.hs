 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TemplateHaskell #-}

module Kernel.Mock.Exceptions where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

newtype OrderError = OrderNotFound Text
  deriving (Show, IsBecknAPIError, Typeable)

instanceExceptionWithParent 'HTTPException ''OrderError

instance IsBaseError OrderError where
  toMessage = \case
    OrderNotFound orderId -> Just $ "Order not found:" <> show orderId

instance IsHTTPError OrderError where
  toErrorCode = \case
    OrderNotFound _ -> "ORDER_NOT_FOUND"
  toHttpCode _ = E500

instance IsAPIError OrderError

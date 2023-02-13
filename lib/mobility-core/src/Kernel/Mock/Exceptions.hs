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

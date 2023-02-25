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

module Kernel.Storage.Hedis.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data HedisError
  = HedisReplyError String
  | HedisDecodeError Text
  | HedisTransactionAborted
  deriving (Show, Typeable, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HedisError

instance IsBaseError HedisError where
  toMessage = \case
    HedisReplyError err -> Just $ show err
    HedisDecodeError err -> Just err
    HedisTransactionAborted -> Nothing

instance IsHTTPError HedisError where
  toErrorCode = \case
    HedisReplyError _ -> "REDIS_REPLY_ERROR"
    HedisDecodeError _ -> "REDIS_DECODE_ERROR"
    HedisTransactionAborted -> "REDIS_TRANSACTION_ABORTED"
  toHttpCode _ = E500

instance IsAPIError HedisError

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Notification.FCM.Error where

import Kernel.Prelude hiding (error)

newtype FcmError = FcmError
  { error :: Maybe ErrorRes
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ErrorRes = ErrorRes
  { code :: Maybe Int,
    message :: Maybe String,
    status :: Maybe String,
    details :: Maybe [ErrorDetail]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype ErrorDetail = ErrorDetail
  { errorCode :: Maybe String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data FCMErrorCategory
  = FCMTransientError
  | FCMPermanentError
  | FCMInvalidTokenError
  deriving (Show, Eq)

categorizeErrorCode :: String -> FCMErrorCategory
categorizeErrorCode "UNREGISTERED" = FCMInvalidTokenError
categorizeErrorCode "NOT_FOUND" = FCMInvalidTokenError
categorizeErrorCode "QUOTA_EXCEEDED" = FCMTransientError
categorizeErrorCode "INTERNAL" = FCMTransientError
categorizeErrorCode "UNAVAILABLE" = FCMTransientError
categorizeErrorCode _ = FCMPermanentError

categorizeHttpStatus :: Int -> FCMErrorCategory
categorizeHttpStatus 429 = FCMTransientError
categorizeHttpStatus 500 = FCMTransientError
categorizeHttpStatus 503 = FCMTransientError
categorizeHttpStatus 401 = FCMPermanentError
categorizeHttpStatus 403 = FCMPermanentError
categorizeHttpStatus 404 = FCMInvalidTokenError
categorizeHttpStatus _ = FCMPermanentError

classifyFcmError :: FcmError -> FCMErrorCategory
classifyFcmError (FcmError Nothing) = FCMPermanentError
classifyFcmError (FcmError (Just errRes)) =
  case errRes.details >>= listToMaybe of
    Just (ErrorDetail (Just code)) -> categorizeErrorCode code
    _ -> maybe FCMPermanentError categorizeHttpStatus errRes.code

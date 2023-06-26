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

module Kernel.External.AadhaarVerification.Gridline.Error where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))

newtype GridlineErrorResp = GridlineErrorResp
  { _error :: GridlineErrorRespBody
  }
  deriving (Eq, Generic, Show, ToJSON)

instance FromJSON GridlineErrorResp where
  parseJSON = withObject "GridlineErrorResp" $ \v -> do
    _error <- v .: "error"
    return (GridlineErrorResp _error)

newtype GridlineErrorRespBody = GridlineErrorRespBody
  { code :: Text
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data GridlineError
  = InvalidAadhaar
  | OtpAlreadySent
  | InvalidRequest
  | SomethingWentWrong
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GridlineError

instance FromResponse GridlineError where
  fromResponse resp = do
    let mRespBody = decode $ responseBody resp
    case mRespBody of
      Just (respBody :: GridlineErrorResp) ->
        case respBody._error.code of
          "INVALID_AADHAAR" -> Just InvalidAadhaar
          "OTP_ALREADY_SENT" -> Just OtpAlreadySent
          _ -> Just InvalidRequest
      Nothing -> Just SomethingWentWrong

instance IsBaseError GridlineError where
  toMessage = \case
    InvalidAadhaar -> Just "Invalid Aadhaar Number."
    OtpAlreadySent -> Just "OTP already sent. Please try after 60 seconds."
    InvalidRequest -> Just "Invalid Request"
    SomethingWentWrong -> Just "Something went wrong"

instance IsHTTPError GridlineError where
  toErrorCode = \case
    InvalidAadhaar -> "INVALID_AADHAAR"
    OtpAlreadySent -> "OTP_ALREADY_SENT"
    InvalidRequest -> "INVALID_REQUEST"
    SomethingWentWrong -> "SOMETHING_WENT_WRONG"

  toHttpCode = \case
    InvalidAadhaar -> E400
    OtpAlreadySent -> E400
    InvalidRequest -> E400
    SomethingWentWrong -> E500

instance IsAPIError GridlineError

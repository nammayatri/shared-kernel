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
import Kernel.Utils.Error.TH (mkOpenAPIError)
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
  | NoMobileNumberRegistered
  | ExceedOtpGenerationLimit
  | AadhaarNumberNotExist
  | InvalidOtp
  | NoShareCode
  | WrongShareCode
  | InvalidShareCode
  | SessionExpired
  | OtpAttemptExceeded
  | UpstreamInternalServerError
  | TransactionAlreadyCompleted
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
          "INVALID_OTP" -> Just InvalidOtp
          "NO_SHARE_CODE" -> Just NoShareCode
          "WRONG_SHARE_CODE" -> Just WrongShareCode
          "INVALID_SHARE_CODE" -> Just InvalidShareCode
          "UPSTREAM_INTERNAL_SERVER_ERROR" -> Just UpstreamInternalServerError
          "TRANSACTION_ALREADY_COMPLETED" -> Just TransactionAlreadyCompleted
          _ -> Just InvalidRequest
      Nothing -> Just SomethingWentWrong

instance IsBaseError GridlineError where
  toMessage = \case
    InvalidAadhaar -> Just "Invalid Aadhaar Number."
    OtpAlreadySent -> Just "OTP already sent. Please try after 60 seconds."
    InvalidRequest -> Just "Invalid Request"
    NoMobileNumberRegistered -> Just "Aadhaar number does not have a mobile number registered with it."
    ExceedOtpGenerationLimit -> Just "Exceeded Maximum OTP generation Limit. Please try again in some time."
    AadhaarNumberNotExist -> Just "Aadhaar number does not exist."
    InvalidOtp -> Just "Invalid OTP"
    NoShareCode -> Just "No share code provided"
    WrongShareCode -> Just "Wrong share code"
    InvalidShareCode -> Just "Invalid share code. Length should be 4 and should only contain numbers."
    SessionExpired -> Just "Session Expired. Please start the process again."
    OtpAttemptExceeded -> Just "OTP attempts exceeded. Please start the process again"
    UpstreamInternalServerError -> Just "Upstream source/Government source internal server error. Please start the process again."
    TransactionAlreadyCompleted -> Just "Transaction already completed. Cannot do further operation on this transaction."
    SomethingWentWrong -> Just "Something went wrong"

instance IsHTTPError GridlineError where
  toErrorCode = \case
    InvalidAadhaar -> "INVALID_AADHAAR"
    OtpAlreadySent -> "OTP_ALREADY_SENT"
    InvalidRequest -> "INVALID_REQUEST"
    NoMobileNumberRegistered -> "NO_MOBILE_NUMBER_REGISTERED"
    ExceedOtpGenerationLimit -> "EXCEED_OTP_GENERATION_LIMIT"
    AadhaarNumberNotExist -> "AADHAAR_NUMBER_NOT_EXIST"
    InvalidOtp -> "INVALID_OTP"
    NoShareCode -> "NO_SHARE_CODE"
    WrongShareCode -> "WRONG_SHARE_CODE"
    InvalidShareCode -> "INVALID_SHARE_CODE"
    SessionExpired -> "SESSION_EXPIRED"
    OtpAttemptExceeded -> "OTP_ATTEMPT_EXCEEDED"
    UpstreamInternalServerError -> "UPSTREAM_INTERNAL_SERVER_ERROR"
    TransactionAlreadyCompleted -> "TRANSACTION_ALREADY_COMPLETED"
    SomethingWentWrong -> "SOMETHING_WENT_WRONG"

  toHttpCode = \case
    InvalidAadhaar -> E400
    OtpAlreadySent -> E400
    InvalidRequest -> E400
    NoMobileNumberRegistered -> E400
    ExceedOtpGenerationLimit -> E400
    AadhaarNumberNotExist -> E400
    InvalidOtp -> E400
    NoShareCode -> E400
    WrongShareCode -> E400
    InvalidShareCode -> E400
    SessionExpired -> E400
    OtpAttemptExceeded -> E400
    UpstreamInternalServerError -> E400
    TransactionAlreadyCompleted -> E400
    SomethingWentWrong -> E500

instance IsAPIError GridlineError

mkOpenAPIError ''GridlineError

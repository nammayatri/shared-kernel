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

module Kernel.External.BackgroundVerification.Checkr.Error where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))

newtype CheckrErrorResp = CheckrErrorResp
  { _error :: [Text]
  }
  deriving (Eq, Generic, Show, ToJSON)

instance FromJSON CheckrErrorResp where
  parseJSON = withObject "CheckrErrorResp" $ \v -> do
    _error <- v .: "error"
    return (CheckrErrorResp _error)

data CheckrError
  = InvalidSSN
  | SSNAlreadyTaken
  | InvalidZipcode
  | InvalidEmail
  | InvalidAge
  | ReportLimitReached
  | SomethingWentWrong Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CheckrError

instance FromResponse CheckrError where
  fromResponse resp = do
    let mRespBody = decode $ responseBody resp
    case mRespBody of
      Just (respBody :: CheckrErrorResp) ->
        case listToMaybe respBody._error of
          Just "SSN is invalid" -> Just InvalidSSN
          Just "SSN is invalid. TIN was provided." -> Just InvalidSSN
          Just "SSN has already been taken" -> Just SSNAlreadyTaken
          Just "Zipcode is invalid" -> Just InvalidZipcode
          Just "Email is not a valid email address" -> Just InvalidEmail
          Just "Candidate must be at least 18 years old" -> Just InvalidAge
          Just "Candidate has reached the limit of Reports allowed" -> Just ReportLimitReached
          Just err -> Just $ SomethingWentWrong err
          Nothing -> Just $ SomethingWentWrong "UNKNOWN_ERROR"
      Nothing -> Just $ SomethingWentWrong "UNKNOWN_ERROR"

instance IsBaseError CheckrError where
  toMessage = \case
    InvalidSSN -> Just "SSN is invalid"
    SSNAlreadyTaken -> Just "SSN has already been taken"
    InvalidZipcode -> Just "Zipcode is invalid"
    InvalidEmail -> Just "Email is not a valid email address"
    InvalidAge -> Just "Candidate must be at least 18 years old"
    ReportLimitReached -> Just "Candidate has reached the limit of Reports allowed"
    SomethingWentWrong err -> Just $ "Something went wrong. Error: " <> err

instance IsHTTPError CheckrError where
  toErrorCode = \case
    InvalidSSN -> "INVALID_SSN"
    SSNAlreadyTaken -> "SSN_ALREADY_TAKEN"
    InvalidZipcode -> "INVALID_ZIPCODE"
    InvalidEmail -> "INVALID_EMAIL"
    InvalidAge -> "INVALID_AGE"
    ReportLimitReached -> "REPORT_LIMIT_REACHED"
    SomethingWentWrong _ -> "SOMETHING_WENT_WRONG"
  toHttpCode = \case
    InvalidSSN -> E400
    SSNAlreadyTaken -> E400
    InvalidZipcode -> E400
    InvalidEmail -> E400
    InvalidAge -> E400
    ReportLimitReached -> E400
    SomethingWentWrong _ -> E500

instance IsAPIError CheckrError

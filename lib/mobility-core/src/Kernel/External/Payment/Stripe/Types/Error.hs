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

module Kernel.External.Payment.Stripe.Types.Error where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Char (toUpper)
import Data.List (intercalate)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Kernel.Utils.JSON
import Servant.Client (ResponseF (responseBody))
import qualified Text.Show

newtype StripeErrorResp = StripeErrorResp
  { _error :: StripeErrorBody
  }
  deriving (Eq, Generic, Show)

instance FromJSON StripeErrorResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON StripeErrorResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data StripeErrorBody = StripeErrorBody
  { _type :: Text,
    _code :: Maybe Text,
    _message :: Maybe Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON StripeErrorBody where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON StripeErrorBody where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

{-
api_error:	API errors cover any other type of problem (e.g., a temporary problem with Stripe’s servers), and are extremely uncommon.
card_error:	Card errors are the most common type of error you should expect to handle. They result when the user enters a card that can’t be charged for some reason.
idempotency_error:	Idempotency errors occur when an Idempotency-Key is re-used on a request that does not match the first request’s API endpoint and parameters.
invalid_request_error:	Invalid request errors arise when your request has invalid parameters.
source: https://docs.stripe.com/api/errors
-}
data StripeError
  = ApiError StripeErrorInfo -- issue from stripe side. ideally shouldn't happen, but if it does, just cancel booking and show user something went wrong please try again
  | CardError StripeErrorInfo -- issue from user side, ask user to retry with another card and can show proper error message too to the user
  | IdempotencyError StripeErrorInfo -- not sure when it will come. ideally shouldn't happen, but if it does, just cancel booking and show user something went wrong please try again
  | InvalidRequestError StripeErrorInfo -- issue from our side. ideally shouldn't happen, but if it does, just cancel booking and show user something went wrong please try again
  | SomethingWentWrong Text -- ideally shouldn't happen, but if it does, just cancel booking and show user something went wrong please try again
  deriving (Eq, Show, IsBecknAPIError)

data StripeErrorInfo = StripeErrorInfo
  { errorCode :: Maybe Text,
    errorMessage :: Maybe Text
  }
  deriving (Eq, Generic)

instance Show StripeErrorInfo where
  show (StripeErrorInfo code msg) =
    maybe (showCode code) T.unpack msg
    where
      showCode :: Maybe Text -> String
      showCode = maybe "" snakeToHuman

      snakeToHuman :: Text -> String
      snakeToHuman = intercalate " " . map (capitalize . show) . T.splitOn "_"

      capitalize :: [Char] -> [Char]
      capitalize [] = []
      capitalize (x : xs) = toUpper x : xs

instanceExceptionWithParent 'HTTPException ''StripeError

instance FromResponse StripeError where
  fromResponse resp = do
    let mRespBody = decode $ responseBody resp
    case mRespBody of
      Just (resp_ :: StripeErrorResp) -> do
        let respBody = resp_._error
        let errorInfo = StripeErrorInfo respBody._code respBody._message
        case respBody._type of
          "api_error" -> Just $ ApiError errorInfo
          "card_error" -> Just $ CardError errorInfo
          "idempotency_error" -> Just $ IdempotencyError errorInfo
          "invalid_request_error" -> Just $ InvalidRequestError errorInfo
          _ -> Just $ SomethingWentWrong $ fromMaybe "Something went wrong" (respBody._message <|> respBody._code)
      _ -> Just $ SomethingWentWrong "Something went wrong"

instance IsBaseError StripeError where
  toMessage = \case
    ApiError info -> Just $ "API Error: " <> show info
    CardError info -> Just $ "Card Error: " <> show info
    IdempotencyError info -> Just $ "Idempotency Error: " <> show info
    InvalidRequestError info -> Just $ "Invalid Request Error: " <> show info
    SomethingWentWrong msg -> Just msg

instance IsHTTPError StripeError where
  toErrorCode = \case
    ApiError _ -> "STRIPE_API_ERROR"
    CardError _ -> "STRIPE_CARD_ERROR"
    IdempotencyError _ -> "STRIPE_IDEMPOTENCY_ERROR"
    InvalidRequestError _ -> "STRIPE_INVALID_REQUEST_ERROR"
    SomethingWentWrong _ -> "STRIPE_INTERNAL_ERROR"

  toHttpCode = \case
    ApiError _ -> E500
    CardError _ -> E400
    IdempotencyError _ -> E400
    InvalidRequestError _ -> E400
    SomethingWentWrong _ -> E500

instance IsAPIError StripeError

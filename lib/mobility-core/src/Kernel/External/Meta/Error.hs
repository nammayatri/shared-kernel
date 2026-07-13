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

module Kernel.External.Meta.Error where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Kernel.External.Meta.Types (metaAesonOptions)
import Kernel.Prelude hiding (error)
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))

-- Meta wraps API failures as {"error": {...}}.
newtype MetaErrorResp = MetaErrorResp
  { error :: MetaErrorBody
  }
  deriving (Eq, Show, Generic)

instance FromJSON MetaErrorResp where
  parseJSON = genericParseJSON metaAesonOptions

instance ToJSON MetaErrorResp where
  toJSON = genericToJSON metaAesonOptions

data MetaErrorBody = MetaErrorBody
  { message :: Maybe Text,
    type_ :: Maybe Text,
    code :: Maybe Int,
    errorSubcode :: Maybe Int,
    errorData :: Maybe Value,
    fbtraceId :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON MetaErrorBody where
  parseJSON = genericParseJSON metaAesonOptions

instance ToJSON MetaErrorBody where
  toJSON = genericToJSON metaAesonOptions

data MetaErrorInfo = MetaErrorInfo
  { errorCode :: Maybe Int,
    errorSubcode :: Maybe Int,
    errorMessage :: Maybe Text,
    fbtraceId :: Maybe Text
  }
  deriving (Eq, Show)

data MetaError
  = MetaRateLimitHit MetaErrorInfo -- 130429 throughput / 80007 account / 4 app        RETRYABLE
  | MetaPairRateLimitHit MetaErrorInfo -- 131056 pair rate limit / 131049 marketing lim RETRYABLE
  | MetaReengagementWindowClosed MetaErrorInfo -- 131047: >24h — needs template          PERMANENT
  | MetaRecipientUnavailable MetaErrorInfo -- 131026 undeliverable                        PERMANENT
  | MetaInvalidRequest MetaErrorInfo -- 100 / 131008 / 131009 param errors                PERMANENT
  | MetaAuthError MetaErrorInfo -- 0 / 190 / 3 / 10 token-permission                      PERMANENT until fixed
  | MetaUnknownError MetaErrorInfo -- everything else incl. undecodable body
  deriving (Eq, Show, IsBecknAPIError)

metaErrorMessage :: MetaErrorInfo -> Text
metaErrorMessage info =
  fromMaybe "unknown" info.errorMessage
    <> maybe "" (\c -> " [code " <> show c <> "]") info.errorCode

classifyMetaError :: Maybe Int -> MetaErrorInfo -> MetaError
classifyMetaError mCode info = case mCode of
  Just c
    | c `elem` [130429, 80007, 4] -> MetaRateLimitHit info
    | c `elem` [131056, 131049] -> MetaPairRateLimitHit info
    | c == 131047 -> MetaReengagementWindowClosed info
    | c == 131026 -> MetaRecipientUnavailable info
    | c `elem` [100, 131008, 131009] -> MetaInvalidRequest info
    | c `elem` [0, 190, 3, 10] -> MetaAuthError info
  _ -> MetaUnknownError info

instanceExceptionWithParent 'HTTPException ''MetaError

instance FromResponse MetaError where
  -- Always return Just so extractApiError never degrades to a RawError:
  -- decode the {"error":{...}} body if possible, else fall back to MetaUnknownError.
  fromResponse resp = Just $ case decode (responseBody resp) :: Maybe MetaErrorResp of
    Just (MetaErrorResp b) ->
      classifyMetaError b.code (MetaErrorInfo b.code b.errorSubcode b.message b.fbtraceId)
    -- Undecodable body: keep a truncated raw snippet so unexpected Meta
    -- responses remain diagnosable in production.
    Nothing ->
      MetaUnknownError
        (MetaErrorInfo Nothing Nothing (Just ("undecodable Meta response body: " <> show (LBS.take 500 (responseBody resp)))) Nothing)

isRetryableMetaError :: MetaError -> Bool
isRetryableMetaError = \case
  MetaRateLimitHit _ -> True
  MetaPairRateLimitHit _ -> True
  _ -> False

needsTemplateReopen :: MetaError -> Bool
needsTemplateReopen = \case
  MetaReengagementWindowClosed _ -> True
  _ -> False

instance IsBaseError MetaError where
  toMessage = \case
    MetaRateLimitHit info -> Just $ "Meta throughput/rate limit: " <> metaErrorMessage info
    MetaPairRateLimitHit info -> Just $ "Meta pair rate limit: " <> metaErrorMessage info
    MetaReengagementWindowClosed info -> Just $ "Meta 24h re-engagement window closed (needs template): " <> metaErrorMessage info
    MetaRecipientUnavailable info -> Just $ "Meta recipient unavailable: " <> metaErrorMessage info
    MetaInvalidRequest info -> Just $ "Meta invalid request: " <> metaErrorMessage info
    MetaAuthError info -> Just $ "Meta auth/permission error: " <> metaErrorMessage info
    MetaUnknownError info -> Just $ "Meta error: " <> metaErrorMessage info

instance IsHTTPError MetaError where
  toErrorCode = \case
    MetaRateLimitHit _ -> "META_RATE_LIMIT_HIT"
    MetaPairRateLimitHit _ -> "META_PAIR_RATE_LIMIT_HIT"
    MetaReengagementWindowClosed _ -> "META_REENGAGEMENT_WINDOW_CLOSED"
    MetaRecipientUnavailable _ -> "META_RECIPIENT_UNAVAILABLE"
    MetaInvalidRequest _ -> "META_INVALID_REQUEST"
    MetaAuthError _ -> "META_AUTH_ERROR"
    MetaUnknownError _ -> "META_UNKNOWN_ERROR"
  toHttpCode = \case
    MetaRateLimitHit _ -> E503
    MetaPairRateLimitHit _ -> E503
    MetaReengagementWindowClosed _ -> E400
    MetaRecipientUnavailable _ -> E400
    MetaInvalidRequest _ -> E400
    MetaAuthError _ -> E401
    MetaUnknownError _ -> E500

instance IsAPIError MetaError

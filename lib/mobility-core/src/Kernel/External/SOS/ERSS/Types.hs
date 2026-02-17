{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SOS.ERSS.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData (ToHttpApiData (..))

-- | OAuth Token Request for password grant (initial authentication)
data ERSSPasswordGrantReq = ERSSPasswordGrantReq
  { -- | Always "password"
    grant_type :: Text,
    client_id :: Text,
    client_secret :: Text,
    username :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSPasswordGrantReq

instance ToForm ERSSPasswordGrantReq where
  toForm ERSSPasswordGrantReq {..} =
    [ ("grant_type", toQueryParam grant_type),
      ("client_id", toQueryParam client_id),
      ("client_secret", toQueryParam client_secret),
      ("username", toQueryParam username),
      ("password", toQueryParam password)
    ]

-- | OAuth Token Request for refresh grant
data ERSSRefreshGrantReq = ERSSRefreshGrantReq
  { -- | Always "refresh_token"
    grant_type :: Text,
    client_id :: Text,
    client_secret :: Text,
    refresh_token :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSRefreshGrantReq

instance ToForm ERSSRefreshGrantReq where
  toForm ERSSRefreshGrantReq {..} =
    [ ("grant_type", toQueryParam grant_type),
      ("client_id", toQueryParam client_id),
      ("client_secret", toQueryParam client_secret),
      ("refresh_token", toQueryParam refresh_token)
    ]

-- | OAuth Token Response from Keycloak
data ERSSAuthResponse = ERSSAuthResponse
  { access_token :: Text,
    refresh_token :: Text,
    -- | Access token TTL in seconds
    expires_in :: Int,
    -- | Refresh token TTL in seconds
    refresh_expires_in :: Int,
    -- | Usually "Bearer"
    token_type :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ERSSAuthResponse

instance ToJSON ERSSAuthResponse

-- | Initial SOS Request matching C-DAC API spec
data ERSSInitialSOSReq = ERSSInitialSOSReq
  { -- | Unique ID from source (optional)
    id :: Maybe Text,
    -- | "YYYY-MM-DD HH:MM:SS"
    dateTime :: Text,
    -- | User's latitude
    latitude :: Double,
    -- | User's longitude
    longitude :: Double,
    -- | Speed if available
    speed :: Maybe Double,
    -- | User's mobile number (mandatory)
    mobileNo :: Text,
    -- | Device IMEI
    imeiNo :: Maybe Text,
    -- | GPS provider name
    gpsProvider :: Maybe Text,
    -- | User's name
    senderName :: Maybe Text,
    -- | User's address
    address :: Maybe Text,
    -- | GPS accuracy in meters
    gpsAccuracy :: Maybe Double,
    -- | State code assigned by C-DAC
    stateCode :: Maybe Text,
    -- | "true"/"false" - can operator call?
    silentCommunication :: Maybe Text,
    -- | Special assistance requirements
    specialNeeds :: Maybe Text,
    -- | Date of birth "YYYY-MM-DD"
    dob :: Maybe Text,
    -- | "MALE"/"FEMALE"/"OTHERS"
    gender :: Maybe Text,
    -- | Media file name or URL
    attachmentFileName :: Maybe Text,
    -- | Source identifier (mandatory)
    authId :: Text,
    -- | Source verification code (mandatory)
    authCode :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSInitialSOSReq

instance FromJSON ERSSInitialSOSReq

instance ToSchema ERSSInitialSOSReq

-- | Generic ERSS API Response envelope matching C-DAC API spec.
-- All ERSS APIs return this envelope structure:
--   resultCode: "OPERATION_SUCCESS" or "OPERATION_FAILURE"
--   resultString: success description or null
--   errorMsg: error description on failure, null on success
--   message: additional message (optional)
--   payLoad: API-specific payload on success, null on failure
data ERSSApiResponse a = ERSSApiResponse
  { resultCode :: Text,
    resultString :: Maybe Text,
    errorMsg :: Maybe Text,
    message :: Maybe Text,
    payLoad :: Maybe a
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (ERSSApiResponse a)

instance (ToJSON a) => ToJSON (ERSSApiResponse a)

instance (ToSchema a) => ToSchema (ERSSApiResponse a)

-- | Payload for Initial SOS success response
-- Contains: signalId (reference ID at ERSS), status (STARTED), remarks
data ERSSSOSPayload = ERSSSOSPayload
  { signalId :: Int,
    status :: Text,
    remarks :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ERSSSOSPayload

instance ToJSON ERSSSOSPayload

instance ToSchema ERSSSOSPayload

-- | Payload for SOS Trace success response
-- Contains: signalId, status (SOS_PROCESSED/SOS_CLOSED), statusUpdateTime, remarks
data ERSSTracePayload = ERSSTracePayload
  { signalId :: Int,
    status :: Text,
    statusUpdateTime :: Maybe Text,
    remarks :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ERSSTracePayload

instance ToJSON ERSSTracePayload

instance ToSchema ERSSTracePayload

-- | Type alias: Initial SOS Response from C-DAC
type ERSSInitialSOSRes = ERSSApiResponse ERSSSOSPayload

-- | SOS Trace Request for location updates
data ERSSTraceReq = ERSSTraceReq
  { -- | ID from Initial SOS response
    trackingId :: Text,
    -- | "YYYY-MM-DD HH:MM:SS"
    dateTime :: Text,
    latitude :: Double,
    longitude :: Double,
    speed :: Maybe Double,
    gpsAccuracy :: Maybe Double,
    authId :: Text,
    authCode :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSTraceReq

instance FromJSON ERSSTraceReq

instance ToSchema ERSSTraceReq

-- | Type alias: SOS Trace Response from C-DAC
type ERSSTraceRes = ERSSApiResponse ERSSTracePayload

-- | Status Update Request
data ERSSStatusUpdateReq = ERSSStatusUpdateReq
  { trackingId :: Text,
    -- | Status to update
    status :: Text,
    authId :: Text,
    authCode :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSStatusUpdateReq

instance FromJSON ERSSStatusUpdateReq

instance ToSchema ERSSStatusUpdateReq

-- | Type alias: Status Update Response from C-DAC
type ERSSStatusUpdateRes = ERSSApiResponse Value

-- | ERSS API Error types matching C-DAC API failure response format.
-- C-DAC failure responses follow this structure:
-- { "resultCode": "OPERATION_FAILURE", "resultString": null,
--   "errorMsg": "<description>", "payLoad": null }
data ERSSError
  = -- | API returned OPERATION_FAILURE with errorMsg
    ERSSOperationFailure Text
  | -- | Authentication/authorization errors (Keycloak or API-level)
    ERSSAuthError Text
  | -- | Fallback for unparseable or unexpected errors
    ERSSUnknownError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ERSSError

instance IsBaseError ERSSError where
  toMessage = \case
    ERSSOperationFailure msg -> Just $ "ERSS Operation Failed: " <> msg
    ERSSAuthError msg -> Just $ "ERSS Auth Error: " <> msg
    ERSSUnknownError msg -> Just $ "ERSS Error: " <> msg

instance IsHTTPError ERSSError where
  toErrorCode = \case
    ERSSOperationFailure _ -> "ERSS_OPERATION_FAILURE"
    ERSSAuthError _ -> "ERSS_AUTH_ERROR"
    ERSSUnknownError _ -> "ERSS_UNKNOWN_ERROR"

  toHttpCode = \case
    ERSSOperationFailure _ -> E500
    ERSSAuthError _ -> E401
    ERSSUnknownError _ -> E500

instance IsAPIError ERSSError

-- | Parse error from HTTP failure responses.
-- Handles both C-DAC ERSS envelope format and Keycloak OAuth error format.
instance FromResponse ERSSError where
  fromResponse resp = do
    let body = responseBody resp
    case decode body of
      Just err -> Just err
      Nothing -> Just $ ERSSUnknownError "Failed to parse ERSS error response"

-- | Parse from C-DAC ERSS API error envelope:
--   { "resultCode": "OPERATION_FAILURE", "errorMsg": "Unauthorized Access", ... }
-- Also handles Keycloak OAuth errors:
--   { "error": "unauthorized_client", "error_description": "Invalid client secret" }
instance FromJSON ERSSError where
  parseJSON = withObject "ERSSError" $ \o -> do
    -- Try C-DAC ERSS API error format
    mResultCode <- o .:? "resultCode"
    mErrorMsg <- o .:? "errorMsg"
    -- Try Keycloak OAuth error format
    mKeycloakErr <- o .:? "error"
    mKeycloakDesc <- o .:? "error_description"
    case (mResultCode :: Maybe Text) of
      Just "OPERATION_FAILURE" ->
        pure $ ERSSOperationFailure (fromMaybe "Unknown error" mErrorMsg)
      _ -> case (mKeycloakErr :: Maybe Text) of
        Just err ->
          pure $ ERSSAuthError (err <> maybe "" (": " <>) mKeycloakDesc)
        Nothing ->
          pure $ ERSSUnknownError (fromMaybe (fromMaybe "Unknown error" mKeycloakDesc) mErrorMsg)

instance ToJSON ERSSError where
  toJSON err =
    object
      [ "resultCode" .= ("OPERATION_FAILURE" :: Text),
        "resultString" .= Null,
        "errorMsg" .= toErrMsg err,
        "payLoad" .= Null
      ]
    where
      toErrMsg = \case
        ERSSOperationFailure msg -> msg
        ERSSAuthError msg -> msg
        ERSSUnknownError msg -> msg

-- | Check if an ERSS API response indicates success
isERSSSuccess :: ERSSApiResponse a -> Bool
isERSSSuccess res = res.resultCode == "OPERATION_SUCCESS"

-- | Bearer token wrapper for Authorization header
newtype ERSSAuthToken = ERSSAuthToken Text
  deriving (Show, Eq, Generic)

instance ToHttpApiData ERSSAuthToken where
  toUrlPiece (ERSSAuthToken token) = "Bearer " <> token

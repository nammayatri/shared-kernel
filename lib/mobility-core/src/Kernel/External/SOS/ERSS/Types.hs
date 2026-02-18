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
  { grant_type :: Text,
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
  { grant_type :: Text,
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
    expires_in :: Int,
    refresh_expires_in :: Int,
    token_type :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ERSSAuthResponse

instance ToJSON ERSSAuthResponse

-- | Initial SOS Request matching C-DAC API spec
data ERSSInitialSOSReq = ERSSInitialSOSReq
  { id :: Maybe Text,
    dateTime :: Text,
    latitude :: Double,
    longitude :: Double,
    speed :: Maybe Double,
    mobileNo :: Text,
    imeiNo :: Maybe Text,
    gpsProvider :: Maybe Text,
    senderName :: Maybe Text,
    address :: Maybe Text,
    gpsAccuracy :: Maybe Double,
    stateCode :: Maybe Text,
    silentCommunication :: Maybe Text,
    specialNeeds :: Maybe Text,
    dob :: Maybe Text,
    gender :: Maybe Text,
    attachmentFileName :: Maybe Text,
    authId :: Text,
    authCode :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSInitialSOSReq

instance FromJSON ERSSInitialSOSReq

instance ToSchema ERSSInitialSOSReq

-- | Generic ERSS API Response envelope (resultCode, resultString, errorMsg, message, payLoad)
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

-- | SOS Trace Request for location updates (PACKET field)
newtype ERSSTraceReq = ERSSTraceReq
  { packet :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ERSSTraceReq where
  toJSON (ERSSTraceReq pkt) = object ["PACKET" .= pkt]

instance FromJSON ERSSTraceReq where
  parseJSON = withObject "ERSSTraceReq" $ \o ->
    ERSSTraceReq <$> o .: "PACKET"

instance ToSchema ERSSTraceReq

-- | Type alias: SOS Trace Response from C-DAC
type ERSSTraceRes = ERSSApiResponse ERSSTracePayload

-- | Inbound Status Update from C-DAC (Section 7)
data ERSSStatusUpdateReq = ERSSStatusUpdateReq
  { idSource :: Maybe Text,
    idErss :: Maybe Text,
    currentStatus :: Text,
    statusDesc :: Maybe Text,
    comments :: Maybe Text,
    lastUpdatedTime :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ERSSStatusUpdateReq

instance ToJSON ERSSStatusUpdateReq

instance ToSchema ERSSStatusUpdateReq

-- | Type alias: Status Update Response (acknowledgment we send back to C-DAC)
type ERSSStatusUpdateRes = ERSSApiResponse Value

-- | Type alias: Media Upload Response from C-DAC
type ERSSMediaUploadRes = ERSSApiResponse Value

-- | ERSS API Error types matching C-DAC API failure response format.
-- C-DAC failure responses follow this structure:
-- { "resultCode": "OPERATION_FAILURE", "resultString": null,
--   "errorMsg": "<description>", "payLoad": null }
data ERSSError
  = ERSSOperationFailure Text
  | ERSSAuthError Text
  | ERSSUnknownError Text
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

instance FromResponse ERSSError where
  fromResponse resp = do
    let body = responseBody resp
    case decode body of
      Just err -> Just err
      Nothing -> Just $ ERSSUnknownError "Failed to parse ERSS error response"

instance FromJSON ERSSError where
  parseJSON = withObject "ERSSError" $ \o -> do
    mResultCode <- o .:? "resultCode"
    mErrorMsg <- o .:? "errorMsg"
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

isERSSSuccess :: ERSSApiResponse a -> Bool
isERSSSuccess res = res.resultCode == "OPERATION_SUCCESS"

-- | Bearer token for Authorization header
newtype ERSSAuthToken = ERSSAuthToken Text
  deriving (Show, Eq, Generic)

instance ToHttpApiData ERSSAuthToken where
  toUrlPiece (ERSSAuthToken token) = "Bearer " <> token

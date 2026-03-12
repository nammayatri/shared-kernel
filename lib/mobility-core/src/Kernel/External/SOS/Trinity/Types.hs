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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SOS.Trinity.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))
import Web.Internal.HttpApiData (ToHttpApiData (..))

-- | Authentication request
data TrinityAuthReq = TrinityAuthReq
  { authorizationKey :: Text,
    username :: Text,
    password :: Text,
    grantType :: Text,
    scope :: Text,
    apim :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Authentication response
data TrinityAuthRes = TrinityAuthRes
  { access_token :: Text,
    refresh_token :: Maybe Text,
    scope :: Maybe Text,
    token_type :: Maybe Text,
    expires_in :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | SOS Trigger request with mixed-case JSON keys matching the Trinity API spec
data TrinitySOSReq = TrinitySOSReq
  { clientId :: Maybe Text,
    clientCode :: Maybe Text,
    name :: Text,
    city :: Maybe Text,
    address :: Maybe Text,
    deviceUuid :: Maybe Text,
    email :: Maybe Text,
    relativeName1 :: Maybe Text,
    relativeName2 :: Maybe Text,
    relativeContact1 :: Maybe Text,
    relativeContact2 :: Maybe Text,
    gender :: Maybe Text,
    simNo :: Text,
    datetime :: Maybe Text,
    emergencyMessage :: Maybe Text,
    latitude :: Text,
    longitude :: Text,
    videoPath :: Maybe Text,
    driverName :: Maybe Text,
    driverContactNo :: Maybe Text,
    vehicleNo :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehLat :: Text,
    vehLng :: Text,
    deviceType :: Int,
    vehLocUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema)

instance ToJSON TrinitySOSReq where
  toJSON TrinitySOSReq {..} =
    object $
      filter
        ((/= Null) . snd)
        [ "client_id" .= clientId,
          "client_code" .= clientCode,
          "Name" .= name,
          "City" .= city,
          "Address" .= address,
          "Device_UUID" .= deviceUuid,
          "Email" .= email,
          "Relative_Name1" .= relativeName1,
          "Relative_Name2" .= relativeName2,
          "Relative_Contact1" .= relativeContact1,
          "Relative_Contact2" .= relativeContact2,
          "Gender" .= gender,
          "SIM_No" .= simNo,
          "Datetime" .= datetime,
          "Emergency_Message" .= emergencyMessage,
          "Latitude" .= latitude,
          "Longitude" .= longitude,
          "VideoPath" .= videoPath,
          "Driver_name" .= driverName,
          "Driver_ContactNo" .= driverContactNo,
          "Vehicle_No" .= vehicleNo,
          "Vehicle_Model" .= vehicleModel,
          "Veh_lat" .= vehLat,
          "Veh_lng" .= vehLng,
          "deviceType" .= deviceType,
          "Veh_loc_url" .= vehLocUrl
        ]

instance FromJSON TrinitySOSReq where
  parseJSON = withObject "TrinitySOSReq" $ \o ->
    TrinitySOSReq
      <$> o .:? "client_id"
      <*> o .:? "client_code"
      <*> o .: "Name"
      <*> o .:? "City"
      <*> o .:? "Address"
      <*> o .:? "Device_UUID"
      <*> o .:? "Email"
      <*> o .:? "Relative_Name1"
      <*> o .:? "Relative_Name2"
      <*> o .:? "Relative_Contact1"
      <*> o .:? "Relative_Contact2"
      <*> o .:? "Gender"
      <*> o .: "SIM_No"
      <*> o .:? "Datetime"
      <*> o .:? "Emergency_Message"
      <*> o .: "Latitude"
      <*> o .: "Longitude"
      <*> o .:? "VideoPath"
      <*> o .:? "Driver_name"
      <*> o .:? "Driver_ContactNo"
      <*> o .:? "Vehicle_No"
      <*> o .:? "Vehicle_Model"
      <*> o .: "Veh_lat"
      <*> o .: "Veh_lng"
      <*> o .: "deviceType"
      <*> o .:? "Veh_loc_url"

-- | SOS Trigger success response
data TrinitySOSRes = TrinitySOSRes
  { caseId :: Maybe Text,
    message :: Maybe Text,
    status :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

isTrinitySuccess :: TrinitySOSRes -> Bool
isTrinitySuccess = (.status)

-- | Trinity API Error types
data TrinityError
  = TrinityOperationFailure Text
  | TrinityAuthError Text
  | TrinityUnknownError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TrinityError

instance IsBaseError TrinityError where
  toMessage = \case
    TrinityOperationFailure msg -> Just $ "Trinity Operation Failed: " <> msg
    TrinityAuthError msg -> Just $ "Trinity Auth Error: " <> msg
    TrinityUnknownError msg -> Just $ "Trinity Error: " <> msg

instance IsHTTPError TrinityError where
  toErrorCode = \case
    TrinityOperationFailure _ -> "TRINITY_OPERATION_FAILURE"
    TrinityAuthError _ -> "TRINITY_AUTH_ERROR"
    TrinityUnknownError _ -> "TRINITY_UNKNOWN_ERROR"

  toHttpCode = \case
    TrinityOperationFailure _ -> E500
    TrinityAuthError _ -> E401
    TrinityUnknownError _ -> E500

instance IsAPIError TrinityError

-- | Generic container for various possible error fields Trinity might send.
data TrinityGenericError = TrinityGenericError
  { message :: Maybe Text,
    errorValue :: Maybe Text, -- JSON key: "error" (avoids clash with Prelude.error)
    error_description :: Maybe Text,
    errorMessage :: Maybe Text,
    msg :: Maybe Text,
    detail :: Maybe Text,
    errorMessageCap :: Maybe Text -- JSON key: "ERROR_MESSAGE"
  }
  deriving (Show, Eq, Generic)

instance FromJSON TrinityGenericError where
  parseJSON = withObject "TrinityGenericError" $ \o ->
    TrinityGenericError
      <$> o .:? "message"
      <*> o .:? "error"
      <*> o .:? "error_description"
      <*> o .:? "errorMessage"
      <*> o .:? "msg"
      <*> o .:? "detail"
      <*> o .:? "ERROR_MESSAGE"

firstJust :: [Maybe a] -> Maybe a
firstJust = \case
  [] -> Nothing
  x : xs -> case x of
    Just v -> Just v
    Nothing -> firstJust xs

instance FromResponse TrinityError where
  fromResponse resp = do
    let body = responseBody resp
    case (decode body :: Maybe TrinityError) of
      Just err -> Just err
      Nothing ->
        case (decode body :: Maybe TrinityGenericError) of
          Just g ->
            let msgText =
                  firstJust
                    [ g.message,
                      g.error_description,
                      g.errorValue,
                      g.errorMessage,
                      g.msg,
                      g.detail,
                      g.errorMessageCap
                    ]
             in Just $ TrinityUnknownError (fromMaybe "Unknown error response" msgText)
          Nothing ->
            Just $ TrinityUnknownError "Failed to parse Trinity error response (non-JSON or empty)"

instance FromJSON TrinityError where
  parseJSON = withObject "TrinityError" $ \o -> do
    mStatus <- o .:? "status"
    mMessage <- o .:? "message"
    mError <- o .:? "Error"
    mErrorDesc <- o .:? "error_description"
    mErrorCode <- o .:? "error"
    case (mStatus :: Maybe Bool) of
      Just False ->
        case (mError :: Maybe Object) of
          Just errObj -> do
            errMsg <- errObj .:? "ERROR_MESSAGE"
            pure $ TrinityAuthError (fromMaybe (fromMaybe "Unknown auth error" mMessage) errMsg)
          Nothing ->
            pure $ TrinityOperationFailure (fromMaybe "Unknown error" mMessage)
      _ ->
        pure $
          TrinityUnknownError $
            fromMaybe "Unknown error" $
              mMessage <|> mErrorDesc <|> (mErrorCode >>= \c -> Just $ "error: " <> c)

instance ToJSON TrinityError where
  toJSON err =
    object ["message" .= toErrMsg err, "status" .= False]
    where
      toErrMsg = \case
        TrinityOperationFailure msg -> msg
        TrinityAuthError msg -> msg
        TrinityUnknownError msg -> msg

-- | Bearer token for Authorization header
newtype TrinityAuthToken = TrinityAuthToken Text
  deriving (Show, Eq, Generic)

instance ToHttpApiData TrinityAuthToken where
  toUrlPiece (TrinityAuthToken token) = "Bearer " <> token

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

module Kernel.External.SOS.GJ112.Types where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Servant.Client (ResponseF (responseBody))
import Web.Internal.HttpApiData (ToHttpApiData (..))

-- | Authentication request (userName and password are Base64-encoded)
data GJ112AuthReq = GJ112AuthReq
  { -- | Base64-encoded username
    userName :: Text,
    -- | Base64-encoded password
    password :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Authentication response from GJ112
data GJ112AuthRes = GJ112AuthRes
  { token :: Text,
    tokenCreationTime :: Integer,
    expiresAt :: Int,
    employeeRole :: Maybe Text,
    employeeName :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GJ112SOSReq = GJ112SOSReq
  { clientId :: Text,
    clientCode :: Text,
    name :: Text,
    city :: Text,
    address :: Maybe Text,
    deviceUuid :: Maybe Text,
    email :: Maybe Text,
    relativeName1 :: Maybe Text,
    relativeName2 :: Maybe Text,
    relativeContact1 :: Maybe Text,
    relativeContact2 :: Maybe Text,
    gender :: Maybe Text,
    simNo :: Maybe Text,
    datetime :: Text,
    emergencyMessage :: Text,
    latitude :: Text,
    longitude :: Text,
    videoPath :: Maybe Text,
    driverName :: Maybe Text,
    driverContactNo :: Maybe Text,
    vehicleNo :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehLat :: Maybe Text,
    vehLng :: Maybe Text,
    deviceType :: Maybe Int,
    vehLocUrl :: Maybe Text,
    vendorName :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleType :: Maybe Text,
    vehicleMake :: Maybe Text,
    vehicleAppearanceNotes :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data GJ112SOSRes = GJ112SOSRes
  { message :: Maybe Text,
    responseCode :: Maybe Int,
    referenceId :: Maybe Integer,
    action :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | GJ112 API Error types.
data GJ112Error
  = GJ112OperationFailure Text
  | GJ112AuthError Text
  | GJ112UnknownError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GJ112Error

instance IsBaseError GJ112Error where
  toMessage = \case
    GJ112OperationFailure msg -> Just $ "GJ112 Operation Failed: " <> msg
    GJ112AuthError msg -> Just $ "GJ112 Auth Error: " <> msg
    GJ112UnknownError msg -> Just $ "GJ112 Error: " <> msg

instance IsHTTPError GJ112Error where
  toErrorCode = \case
    GJ112OperationFailure _ -> "GJ112_OPERATION_FAILURE"
    GJ112AuthError _ -> "GJ112_AUTH_ERROR"
    GJ112UnknownError _ -> "GJ112_UNKNOWN_ERROR"

  toHttpCode = \case
    GJ112OperationFailure _ -> E500
    GJ112AuthError _ -> E401
    GJ112UnknownError _ -> E500

instance IsAPIError GJ112Error

instance FromResponse GJ112Error where
  fromResponse resp = do
    let body = responseBody resp
    case decode body of
      Just err -> Just err
      Nothing -> Just $ GJ112UnknownError "Failed to parse GJ112 error response"

instance FromJSON GJ112Error where
  parseJSON = withObject "GJ112Error" $ \o -> do
    mResponseCode <- o .:? "responseCode"
    mMessage <- o .:? "message"
    mError <- o .:? "error"
    case (mResponseCode :: Maybe Int) of
      Just code
        | code /= 200 ->
          pure $ GJ112OperationFailure (fromMaybe ("Error code: " <> show code) mMessage)
      _ -> case (mError :: Maybe Text) of
        Just err ->
          pure $ GJ112AuthError (err <> maybe "" (": " <>) mMessage)
        Nothing ->
          pure $ GJ112UnknownError (fromMaybe "Unknown error" mMessage)

instance ToJSON GJ112Error where
  toJSON err =
    object ["message" .= toErrMsg err]
    where
      toErrMsg = \case
        GJ112OperationFailure msg -> msg
        GJ112AuthError msg -> msg
        GJ112UnknownError msg -> msg

isGJ112Success :: GJ112SOSRes -> Bool
isGJ112Success res = maybe False (== 200) res.responseCode

-- | Auth token wrapper for Authorization header
newtype GJ112AuthToken = GJ112AuthToken Text
  deriving (Show, Eq, Generic)

instance ToHttpApiData GJ112AuthToken where
  toUrlPiece (GJ112AuthToken token) = token

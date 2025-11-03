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

module Kernel.External.SharedLogic.DigiLocker.Error where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

-- DigiLocker error response JSON format
data DigiLockerErrorResp = DigiLockerErrorResp
  { _error :: Text,
    error_description :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON DigiLockerErrorResp where
  parseJSON = A.genericParseJSON stripPrefixUnderscoreIfAny

instance A.ToJSON DigiLockerErrorResp where
  toJSON = A.genericToJSON stripPrefixUnderscoreIfAny

data DigiLockerError
  = DGLError Text
  | DGLUnauthorizedError -- 401: invalid_token
  | DGLBadRequestError Text -- 400: uri_missing
  | DGLForbiddenError Text -- 403: insufficient_scope
  | DGLNotFoundError Text -- 404: invalid_uri
  | DGLInternalServerError Text -- 530: repository_service_resperror
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DigiLockerError

instance IsBaseError DigiLockerError where
  toMessage = \case
    DGLError msg -> Just $ "DigiLocker Error: " <> msg
    DGLUnauthorizedError -> Just "DigiLocker token expired or has been revoked by DigiLocker user."
    DGLBadRequestError msg -> Just $ "DigiLocker Bad Request: " <> msg
    DGLForbiddenError msg -> Just $ "DigiLocker Forbidden: " <> msg
    DGLNotFoundError msg -> Just $ "DigiLocker Not Found: " <> msg
    DGLInternalServerError msg -> Just $ "DigiLocker Internal Server Error: " <> msg

instance IsHTTPError DigiLockerError where
  toErrorCode = \case
    DGLError _ -> "DGL_ERROR"
    DGLUnauthorizedError -> "DGL_UNAUTHORIZED"
    DGLBadRequestError _ -> "DGL_BAD_REQUEST"
    DGLForbiddenError _ -> "DGL_FORBIDDEN"
    DGLNotFoundError _ -> "DGL_NOT_FOUND"
    DGLInternalServerError _ -> "DGL_INTERNAL_SERVER_ERROR"

  toHttpCode = \case
    DGLUnauthorizedError -> E401
    DGLBadRequestError _ -> E400
    DGLForbiddenError _ -> E403
    DGLNotFoundError _ -> E404
    DGLInternalServerError _ -> E500
    DGLError _ -> E400

instance IsAPIError DigiLockerError

-- Helper function to parse error from response body
parseDigiLockerErrorFromResponse ::
  Int ->
  BS.ByteString ->
  DigiLockerError
parseDigiLockerErrorFromResponse statusCode body =
  case A.decodeStrict body of
    Just (errorResp :: DigiLockerErrorResp) -> do
      let errorDesc = fromMaybe errorResp._error errorResp.error_description
      case statusCode of
        401 -> DGLUnauthorizedError
        400 -> DGLBadRequestError errorDesc
        403 -> DGLForbiddenError errorDesc
        404 -> DGLNotFoundError errorDesc
        530 -> DGLInternalServerError errorDesc
        _ -> DGLError errorDesc
    Nothing -> do
      -- If we can't parse JSON, use status code with generic message
      case statusCode of
        401 -> DGLUnauthorizedError
        400 -> DGLBadRequestError "Bad Request"
        403 -> DGLForbiddenError "Forbidden"
        404 -> DGLNotFoundError "Not Found"
        530 -> DGLInternalServerError "Internal Server Error"
        _ -> DGLError $ "HTTP " <> show statusCode

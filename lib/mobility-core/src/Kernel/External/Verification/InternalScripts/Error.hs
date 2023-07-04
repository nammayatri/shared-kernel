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

module Kernel.External.Verification.InternalScripts.Error where

import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))
import Prelude

data FaceVerificationError
  = FakeFaceDetected
  | FaceVerificationInternalServer
  | PoorImageQuality
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FaceVerificationError

instance FromResponse FaceVerificationError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    422 -> Just FakeFaceDetected
    _ -> Just FaceVerificationInternalServer

instance IsBaseError FaceVerificationError where
  toMessage = \case
    FakeFaceDetected -> Just "Fake face detected. Please provide a valid image."
    FaceVerificationInternalServer -> Just "Could not verify face. Please try again."
    PoorImageQuality -> Just "Could not verify face due to poor image quality"

instance IsHTTPError FaceVerificationError where
  toErrorCode = \case
    FakeFaceDetected -> "FAKE_FACE_DETECTED"
    FaceVerificationInternalServer -> "INTERNAL_SERVER_ERROR"
    PoorImageQuality -> "POOR_IMAGE_QUALITY"

  toHttpCode = \case
    FakeFaceDetected -> E422
    FaceVerificationInternalServer -> E500
    PoorImageQuality -> E422

instance IsAPIError FaceVerificationError

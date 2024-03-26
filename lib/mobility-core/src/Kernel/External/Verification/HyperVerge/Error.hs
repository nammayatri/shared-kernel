{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.HyperVerge.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data HyperVergeError
  = HyperVergeFaceNotDetected
  | HyperVergeRateLimitError
  | HyperVergeInternalError
  | HyperVergeCallError Text Text
  | HyperVergeBadRequest Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HyperVergeError

instance IsBaseError HyperVergeError where
  toMessage = \case
    HyperVergeFaceNotDetected -> Just "Face not detected. Please provide a valid image."
    HyperVergeRateLimitError -> Just "Request rate Limit Exceeded."
    HyperVergeInternalError -> Just "Internal Server Error."
    HyperVergeCallError code msg -> Just $ "Error Response from Hyperverge. Status Code = " <> code <> " message = " <> msg
    HyperVergeBadRequest msg -> Just $ "Bad Request " <> msg

instance IsHTTPError HyperVergeError where
  toErrorCode = \case
    HyperVergeFaceNotDetected -> "FACE_NOT_DETECTED"
    HyperVergeRateLimitError -> "RATE_LIMIT_EXCEEDED"
    HyperVergeInternalError -> "INTERNAL_SERVER_ERROR"
    HyperVergeCallError _ _ -> "HYPERVERGE_CALL_ERROR"
    HyperVergeBadRequest _ -> "BAD_REQUEST"

  toHttpCode = \case
    HyperVergeFaceNotDetected -> E422
    HyperVergeRateLimitError -> E429
    HyperVergeInternalError -> E500
    HyperVergeCallError _ _ -> E400 -- HVTODO: Check if this error code fits accurately.
    HyperVergeBadRequest _ -> E400

instance IsAPIError HyperVergeError

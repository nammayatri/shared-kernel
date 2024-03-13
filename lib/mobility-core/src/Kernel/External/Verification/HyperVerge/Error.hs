{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.HyperVerge.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data HyperVergeFaceVerificationError
  = HyperVergeAPIError Text
  | InvalidCredentials
  | RequestsRateLimitExceeded
  | InternalServerError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HyperVergeFaceVerificationError

instance IsBaseError HyperVergeFaceVerificationError where
  toMessage = \case
    HyperVergeAPIError msg -> Just $ "HyperVerge API error: " <> msg
    InvalidCredentials -> Just "Invalid HyperVerge credentials"
    RequestsRateLimitExceeded -> Just "HyperVerge requests rate limit exceeded"
    InternalServerError -> Just "HyperVerge Internal Server Error"

instance IsHTTPError HyperVergeFaceVerificationError where
  toErrorCode = \case
    HyperVergeAPIError _ -> "HYPERVERGE_API_CALL_ERROR"
    InvalidCredentials -> "HYPERVERGE_INVALID_CREDENTIALS"
    RequestsRateLimitExceeded -> "HYPERVERGE_REQUESTS_RATE_LIMIT_EXCEEDED"
    InternalServerError -> "HYPERVERGE_INTERNAL_SERVER_ERROR"

  toHttpCode = \case
    HyperVergeAPIError _ -> E400
    InvalidCredentials -> E401
    RequestsRateLimitExceeded -> E429
    InternalServerError -> E500

instance IsAPIError HyperVergeFaceVerificationError

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.HyperVerge.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

data HyperVergeError
  = HyperVergeFaceNotDetected
  | HyperVergeCallError Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HyperVergeError

instance IsBaseError HyperVergeError where
  toMessage = \case
    HyperVergeFaceNotDetected -> Just "Face not detected. Please provide a valid image."
    HyperVergeCallError code resp -> Just $ "Error Response from Hyperverge. Status Code = " <> code <> " response from HyperVerge is : " <> resp

instance IsHTTPError HyperVergeError where
  toErrorCode = \case
    HyperVergeFaceNotDetected -> "FACE_NOT_DETECTED"
    HyperVergeCallError _ _ -> "HYPERVERGE_CALL_ERROR"

  toHttpCode = \case
    HyperVergeFaceNotDetected -> E422
    HyperVergeCallError _ _ -> E400 -- HVTODO: Check if this error code fits accurately.

instance IsAPIError HyperVergeError

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.Error.BaseError.HTTPError.APIError where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Kernel.Utils.Servant.Client

newtype APICallError = APICallError APIError
  deriving (Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''APICallError

instance IsBaseError APICallError where
  toMessage (APICallError APIError {..}) =
    Just $
      "Request to own API returned error code " <> errorCode
        <> maybe "" (" with message: " <>) errorMessage

instance IsHTTPError APICallError where
  toErrorCode (APICallError _) = "API_CALL_ERROR"

instance IsAPIError APICallError

callOwnAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callOwnAPI = callApiUnwrappingApiError APICallError

catchOwnAPI ::
  ( HasCallStack,
    MonadCatch m,
    Log m
  ) =>
  m a ->
  (Text -> m a) ->
  m a
catchOwnAPI m f = m `safeCatch` \(APICallError APIError {errorCode}) -> f errorCode

infixl 1 `catchOwnAPI`

toAPIError :: (IsHTTPError e, IsAPIError e) => e -> APIError
toAPIError e =
  APIError
    { errorCode = toErrorCode e,
      errorMessage = toMessageIfNotInternal e,
      errorPayload = toPayload e
    }

 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

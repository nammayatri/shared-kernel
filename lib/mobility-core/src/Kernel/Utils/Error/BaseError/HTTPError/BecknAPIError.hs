 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Servant.Client
import Servant.Client (Client, HasClient)

data BecknAPICallError = BecknAPICallError Text Error
  deriving (Show, IsAPIError, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BecknAPICallError

instance IsBaseError BecknAPICallError where
  toMessage (BecknAPICallError action Error {..}) =
    Just $
      "Beckn " <> action <> " request returned error code " <> code
        <> maybe "" ("with message: " <>) message

instance IsHTTPError BecknAPICallError where
  toErrorCode (BecknAPICallError _ _) = "BECKN_API_CALL_ERROR"

type IsBecknAPI api req res =
  ( HasClient ET.EulerClient api,
    Client ET.EulerClient api ~ (req -> ET.EulerClient res),
    ET.JSONEx res,
    ToJSON res
  )

callBecknAPI ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res
  ) =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m res
callBecknAPI mbManagerSelector errorCodeMb action api baseUrl req =
  callBecknAPI' mbManagerSelector errorCodeMb baseUrl (ET.client api req) action

callBecknAPI' ::
  MonadFlow m =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI m res
callBecknAPI' mbManagerSelector errorCodeMb baseUrl eulerClient name =
  callApiUnwrappingApiError
    (becknAPIErrorToException name)
    mbManagerSelector
    errorCodeMb
    baseUrl
    eulerClient
    name

callPseudoBecknAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callPseudoBecknAPI mbManagerSelector errorCodeMb baseUrl eulerClient name =
  callApiUnwrappingApiError
    (becknAPIErrorToException name)
    mbManagerSelector
    errorCodeMb
    baseUrl
    eulerClient
    name

becknAPIErrorToException :: Text -> BecknAPIError -> BecknAPICallError
becknAPIErrorToException name (BecknAPIError becknErr) = BecknAPICallError name becknErr

toBecknAPIError :: (IsHTTPError e, IsBecknAPIError e) => e -> BecknAPIError
toBecknAPIError e =
  BecknAPIError
    Error
      { _type = toType e,
        code = toErrorCode e,
        path = toPath e,
        message = toMessageIfNotInternal e
      }

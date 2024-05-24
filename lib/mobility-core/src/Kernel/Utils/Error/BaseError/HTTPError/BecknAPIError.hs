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

module Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError where

import qualified Data.HashMap.Strict as HM
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Monitoring.Prometheus.Servant
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
    ToJSON res,
    FromJSON res
  )

callBecknAPI ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api
  ) =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPI mbManagerSelector errorCodeMb action api baseUrl internalEndPointHashMap req = do
  callBecknAPI' mbManagerSelector errorCodeMb (Just internalEndPointHashMap) baseUrl (ET.client api req) action api

callBecknAPI' ::
  MonadFlow m =>
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Maybe (HM.HashMap BaseUrl BaseUrl) ->
  CallAPI m api res
callBecknAPI' mbManagerSelector errorCodeMb internalEndPointHashMap baseUrl eulerClient name api = do
  callApiUnwrappingApiError
    (becknAPIErrorToException name)
    mbManagerSelector
    errorCodeMb
    internalEndPointHashMap
    baseUrl
    eulerClient
    name
    api

callPseudoBecknAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  HM.HashMap BaseUrl BaseUrl ->
  CallAPI env api a
callPseudoBecknAPI mbManagerSelector errorCodeMb internalEndPointHashMap baseUrl eulerClient name api =
  callApiUnwrappingApiError
    (becknAPIErrorToException name)
    mbManagerSelector
    errorCodeMb
    (Just internalEndPointHashMap)
    baseUrl
    eulerClient
    name
    api

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

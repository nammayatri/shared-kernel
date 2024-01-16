{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Callback (withBecknCallbackMig, WithBecknCallbackMig) where

import Control.Lens ((.~))
import qualified Data.HashMap.Strict as HM
import EulerHS.Prelude hiding ((.~))
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as M.Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant.Client

someExceptionToCallbackReqMig :: M.Context.Context -> SomeException -> BecknCallbackReq a
someExceptionToCallbackReqMig context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in BecknCallbackReq
        { contents = Left err,
          context
        }

type WithBecknCallbackMig api callback_success m =
  ( MonadFlow m,
    SanitizedUrl api,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  M.Context.Action ->
  Proxy api ->
  M.Context.Context ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallbackMig ::
  (m () -> m ()) ->
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api callback_success m
withBecknCallbackMig doWithCallback auth actionName api context cbUrl internalEndPointHashMap action = do
  now <- getCurrentTime
  cbAction <-
    M.Context.mapToCbAction actionName
      & fromMaybeM (InternalError $ "Beckn " <> show actionName <> " action doesn't have callback")
  let cbContext =
        context
          & #action .~ cbAction
          & #timestamp .~ UTCTimeRFC3339 now
  forkBecknCallback
    (someExceptionToCallbackReqMig cbContext)
    (BecknCallbackReq cbContext . Right)
    (doWithCallback . void . callBecknAPI auth Nothing (show cbAction) api cbUrl internalEndPointHashMap)
    (show actionName)
    action
  return Ack

forkBecknCallback ::
  (Forkable m, MonadCatch m, Log m) =>
  (SomeException -> result) ->
  (success -> result) ->
  (result -> m ()) ->
  Text ->
  m success ->
  m ()
forkBecknCallback fromError fromSuccess doWithResult actionName action =
  fork actionName $
    try action >>= \case
      Right success -> doWithResult $ fromSuccess success
      Left err -> do
        logError $ "Error executing callback action " <> actionName <> ": " <> show err
        doWithResult $ fromError err

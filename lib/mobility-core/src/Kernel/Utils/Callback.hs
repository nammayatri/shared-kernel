module Kernel.Utils.Callback (withBecknCallbackMig, WithBecknCallbackMig) where

import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as M.Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.Common
import EulerHS.Prelude
import qualified EulerHS.Types as ET
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
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  M.Context.Action ->
  Proxy api ->
  M.Context.Context ->
  BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallbackMig ::
  (m () -> m ()) ->
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api callback_success m
withBecknCallbackMig doWithCallback auth actionName api context cbUrl action = do
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
    (doWithCallback . void . callBecknAPI auth Nothing (show cbAction) api cbUrl)
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

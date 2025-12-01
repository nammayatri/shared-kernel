{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Error.FlowHandling
  ( withFlowHandler,
    withFlowHandler',
    withDashboardFlowHandler,
    withDashboardFlowHandler',
    withFlowHandlerAPI,
    withFlowHandlerAPI',
    withDashboardFlowHandlerAPI,
    withDashboardFlowHandlerAPI',
    withFlowHandlerBecknAPI,
    withFlowHandlerBecknAPI',
    apiHandler,
    becknApiHandler,
    someExceptionToBecknApiError,
    handleIfUp,
    throwServantError,
  )
where

import Control.Concurrent.STM (isEmptyTMVar)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Clock hiding (getCurrentTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.Beam.Types as KBT
import Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Queries.SystemConfigs
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.App
import Kernel.Types.Beckn.Ack
import Kernel.Types.Common
import Kernel.Types.Error as Err
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Flow
import Kernel.Utils.Error.BaseError.HTTPError.APIError (toAPIError)
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (toBecknAPIError)
import Kernel.Utils.Logging
import Kernel.Utils.Text
import Network.HTTP.Types (Header, hContentType)
import Network.HTTP.Types.Header (HeaderName)
import Servant (ServerError (..))

-- we are using find query and setoption here which requires the constraint HasFlowHandlerR  has
-- we will be withFlowHandler only in case db or redis call is required as it has the constraint for db and redis env in HasFlowHandlerR
withFlowHandler ::
  HasFlowHandlerR (FlowR r) r =>
  FlowR r a ->
  FlowHandlerR r a
withFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  liftIO . runFlowR flowRt appEnv $ getAndSetKvConfigs >> flow
  where
    getAndSetKvConfigs = do
      now <- getCurrentTime
      kvConfigLastUpdatedTime <- L.getOption KBT.KvConfigLastUpdatedTime >>= maybe (L.setOption KBT.KvConfigLastUpdatedTime now >> pure now) pure
      kvConfigUpdateFrequency <- L.getOption KBT.KvConfigUpdateFrequency >>= maybe (pure 10) pure
      when (round (diffUTCTime now kvConfigLastUpdatedTime) > kvConfigUpdateFrequency) $
        findById "kv_configs" >>= pure . decodeFromText' @Tables
          >>= maybe (incrementSystemConfigsFailedCounter ("kv_config_decode_failed_" <> schemaName (Proxy :: Proxy BeamSC.SystemConfigsT))) (\result' -> L.setOption KBT.Tables result' >> L.setOption KBT.KvConfigLastUpdatedTime now)

withDashboardFlowHandler ::
  ( HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "dashboardClickhouseCfg" r ClickhouseCfg,
    HasField "dashboardClickhouseEnv" r ClickhouseEnv,
    HasFlowHandlerR (FlowR r) r,
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withDashboardFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  let newappEnv = appEnv{serviceClickhouseCfg = appEnv.dashboardClickhouseCfg, serviceClickhouseEnv = appEnv.dashboardClickhouseEnv}
  liftIO . runFlowR flowRt newappEnv $ getAndSetKvConfigs >> flow
  where
    getAndSetKvConfigs = do
      now <- getCurrentTime
      kvConfigLastUpdatedTime <- L.getOption KBT.KvConfigLastUpdatedTime >>= maybe (L.setOption KBT.KvConfigLastUpdatedTime now >> pure now) pure
      kvConfigUpdateFrequency <- L.getOption KBT.KvConfigUpdateFrequency >>= maybe (pure 10) pure
      when (round (diffUTCTime now kvConfigLastUpdatedTime) > kvConfigUpdateFrequency) $
        findById "kv_configs" >>= pure . decodeFromText' @Tables
          >>= maybe (incrementSystemConfigsFailedCounter ("kv_config_decode_failed_" <> schemaName (Proxy :: Proxy BeamSC.SystemConfigsT))) (\result' -> L.setOption KBT.Tables result' >> L.setOption KBT.KvConfigLastUpdatedTime now)

-- in case of normal flow use withFlowHandler' as it does not have any extra constraints
withFlowHandler' ::
  FlowR r a ->
  FlowHandlerR r a
withFlowHandler' flow = do
  (EnvR flowRt appEnv) <- ask
  liftIO . runFlowR flowRt appEnv $ flow

withDashboardFlowHandler' ::
  ( HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "dashboardClickhouseCfg" r ClickhouseCfg,
    HasField "dashboardClickhouseEnv" r ClickhouseEnv,
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withDashboardFlowHandler' flow = do
  (EnvR flowRt appEnv) <- ask
  let newappEnv = appEnv{serviceClickhouseCfg = appEnv.dashboardClickhouseCfg, serviceClickhouseEnv = appEnv.dashboardClickhouseEnv}
  liftIO . runFlowR flowRt newappEnv $ flow

withFlowHandlerAPI ::
  ( HasFlowHandlerR (FlowR r) r,
    Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withFlowHandlerAPI = withFlowHandler . apiHandler . handleIfUp

withDashboardFlowHandlerAPI ::
  ( HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "dashboardClickhouseCfg" r ClickhouseCfg,
    HasField "dashboardClickhouseEnv" r ClickhouseEnv,
    HasFlowHandlerR (FlowR r) r,
    Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withDashboardFlowHandlerAPI = withDashboardFlowHandler . apiHandler . handleIfUp

-- created this for using it in mock-registry as it does not require any extra constraints
withFlowHandlerAPI' ::
  ( Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withFlowHandlerAPI' = withFlowHandler' . apiHandler . handleIfUp

withDashboardFlowHandlerAPI' ::
  ( HasField "serviceClickhouseCfg" r ClickhouseCfg,
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "dashboardClickhouseCfg" r ClickhouseCfg,
    HasField "dashboardClickhouseEnv" r ClickhouseEnv,
    Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withDashboardFlowHandlerAPI' = withDashboardFlowHandler' . apiHandler . handleIfUp

withFlowHandlerBecknAPI ::
  ( HasFlowHandlerR (FlowR r) r,
    Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r AckResponse ->
  FlowHandlerR r AckResponse
withFlowHandlerBecknAPI = withFlowHandler . becknApiHandler . handleIfUp

-- created this for using it in beckn-gateway as it does not require any extra constraints
withFlowHandlerBecknAPI' ::
  ( Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r AckResponse ->
  FlowHandlerR r AckResponse
withFlowHandlerBecknAPI' = withFlowHandler' . becknApiHandler . handleIfUp

handleIfUp ::
  ( L.MonadFlow m,
    Log m,
    MonadReader r m,
    HasField "isShuttingDown" r (TMVar ()),
    Metrics.CoreMetrics m,
    HasField "url" r (Maybe Text)
  ) =>
  m a ->
  m a
handleIfUp flow = do
  shutdown <- asks (.isShuttingDown)
  shouldRun <- L.runIO $ atomically $ isEmptyTMVar shutdown
  if shouldRun
    then flow
    else throwAPIError ServerUnavailable

apiHandler ::
  ( L.MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  m a ->
  m a
apiHandler = (`catch` someExceptionToAPIErrorThrow)

becknApiHandler ::
  ( L.MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  m a ->
  m a
becknApiHandler = (`catch` someExceptionToBecknApiErrorThrow)

someExceptionToAPIErrorThrow ::
  ( L.MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  SomeException ->
  m a
someExceptionToAPIErrorThrow exc
  | Just (HTTPException err) <- fromException exc = throwAPIError err
  | Just (BaseException err) <- fromException exc =
    throwAPIError . InternalError . fromMaybe (show err) $ toMessage err
  | otherwise = throwAPIError . InternalError $ show exc

someExceptionToBecknApiErrorThrow ::
  ( L.MonadFlow m,
    Log m,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  SomeException ->
  m a
someExceptionToBecknApiErrorThrow exc
  | Just (HTTPException err) <- fromException exc = throwBecknApiError err
  | otherwise =
    throwBecknApiError . InternalError $ show exc

someExceptionToBecknApiError :: SomeException -> BecknAPIError
someExceptionToBecknApiError exc
  | Just (HTTPException err) <- fromException exc = toBecknAPIError err
  | otherwise = toBecknAPIError . InternalError $ show exc

throwAPIError ::
  ( Log m,
    MonadThrow m,
    IsHTTPException e,
    Exception e,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  e ->
  m a
throwAPIError = throwHTTPError toAPIError

throwBecknApiError ::
  ( Log m,
    MonadThrow m,
    IsHTTPException e,
    Exception e,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  e ->
  m a
throwBecknApiError = throwHTTPError toBecknAPIError

throwHTTPError ::
  ( ToJSON j,
    Log m,
    MonadThrow m,
    IsHTTPException e,
    Exception e,
    Metrics.CoreMetrics m,
    MonadReader r m,
    HasField "url" r (Maybe Text)
  ) =>
  (e -> j) ->
  e ->
  m b
throwHTTPError toJsonError err = do
  let someExc = toException err
  let callStackStr = T.pack $ prettyCallStack callStack
  logError $ makeLogSomeException someExc
  logError $ "Callstack: " <> callStackStr
  Metrics.incrementErrorCounter "DEFAULT_ERROR" someExc
  throwServantError (toHttpCode err) (toCustomHeaders err) (toJsonError err)

throwServantError ::
  (ToJSON a, Log m, MonadThrow m) =>
  HttpCode ->
  [Header] ->
  a ->
  m b
throwServantError httpCode customHeaders jsonError = withLogTag "HTTP_ERROR" $ do
  let body = A.encode jsonError
  let serverErr = toServerError httpCode
  throwM
    serverErr
      { errBody = body,
        errHeaders = jsonHeader : customHeaders ++ errHeaders serverErr
      }
  where
    jsonHeader :: (HeaderName, ByteString)
    jsonHeader = (hContentType, "application/json;charset=utf-8")

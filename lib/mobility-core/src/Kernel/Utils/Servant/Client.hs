-- {-# LANGUAGE DerivingVia #-}
-- {-# OPTIONS_GHC -Wno-type-defaults #-}

module Kernel.Utils.Servant.Client where

import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.CallAPIError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Kernel.Utils.Servant.BaseUrl
import Kernel.Utils.Text
import Kernel.Utils.Time
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import GHC.Records.Extra (HasField)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant.Client.Core

newtype HttpClientOptions = HttpClientOptions
  { timeoutMs :: Int
  }
  deriving (Generic, FromDhall)

data RetryCfg = RetryCfg
  { maxRetries :: Int,
    baseCoefficient :: Int
  }
  deriving (Generic, FromDhall)

type HasHttpClientOptions r c = HasField "httpClientOptions" r HttpClientOptions

type HasRetryCfg r c = HasField "retryCfg" r RetryCfg

type HasShortDurationRetryCfg r c = HasField "shortDurationRetryCfg" r RetryCfg

type HasLongDurationRetryCfg r c = HasField "longDurationRetryCfg" r RetryCfg

data RetryType = ShortDurationRetry | LongDurationRetry

type CallAPI' m res res' =
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    ET.JSONEx res,
    ToJSON res
  ) =>
  BaseUrl ->
  ET.EulerClient res ->
  Text ->
  m res'

type CallAPI m res = CallAPI' m res res

callAPI ::
  CallAPI' m res (Either ClientError res)
callAPI = callAPI' Nothing

-- Why do we call L.callAPI' (Just "default") instead of L.callAPI' Nothing?
callAPI' ::
  Maybe ET.ManagerSelector ->
  CallAPI' m res (Either ClientError res)
callAPI' mbManagerSelector baseUrl eulerClient desc =
  withLogTag "callAPI" $ do
    let managerSelector = fromMaybe defaultHttpManager mbManagerSelector
    res <-
      measuringDuration (Metrics.addRequestLatency (showBaseUrlText baseUrl) desc) $
        L.callAPI' (Just managerSelector) baseUrl eulerClient
    case res of
      Right r -> logDebug $ "Ok response: " <> truncateText (decodeUtf8 (A.encode r))
      Left err -> logDebug $ "Error occured during client call: " <> show err
    return res

callApiExtractingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err
  ) =>
  Maybe ET.ManagerSelector ->
  CallAPI' m a (Either (CallAPIError err) a)
callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc =
  callAPI' mbManagerSelector baseUrl eulerClient desc
    <&> extractApiError

callApiUnwrappingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err,
    IsHTTPException exc
  ) =>
  (err -> exc) ->
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI m a
callApiUnwrappingApiError toAPIException mbManagerSelector errorCodeMb baseUrl eulerClient desc =
  callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc
    >>= unwrapEitherCallAPIError errorCodeMb baseUrl toAPIException

defaultHttpManager :: String
defaultHttpManager = "default"

setResponseTimeout :: Int -> Http.ManagerSettings -> Http.ManagerSettings
setResponseTimeout timeout settings =
  settings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

createManagers ::
  ( MonadReader r m,
    HasHttpClientOptions r c,
    MonadFlow m
  ) =>
  Map String Http.ManagerSettings ->
  m (Map String Http.Manager)
createManagers managerSettings = do
  timeout <- asks (.httpClientOptions.timeoutMs)
  liftIO $ managersFromManagersSettings timeout managerSettings

createManagersWithTimeout ::
  ( MonadReader r m,
    HasHttpClientOptions r c,
    MonadFlow m
  ) =>
  Map String Http.ManagerSettings ->
  Maybe Int ->
  m (Map String Http.Manager)
createManagersWithTimeout managerSettings Nothing = createManagers managerSettings
createManagersWithTimeout managerSettings (Just timeout) = liftIO $ managersFromManagersSettings timeout managerSettings

managersFromManagersSettings ::
  Int ->
  Map String Http.ManagerSettings ->
  IO (Map String Http.Manager)
managersFromManagersSettings timeout =
  mapM Http.newManager
    . fmap (setResponseTimeout timeout)
    . Map.insert defaultHttpManager Http.tlsManagerSettings

catchConnectionErrors :: (MonadCatch m, Log m) => m a -> (ExternalAPICallError -> m a) -> m a
catchConnectionErrors action errorHandler =
  action `catch` \err -> do
    case err.clientError of
      ConnectionError _ -> errorHandler err
      _ -> throwError err

retryAction ::
  ( MonadCatch m,
    Metrics.CoreMetrics m,
    MonadIO m,
    Log m
  ) =>
  ExternalAPICallError ->
  Int ->
  Int ->
  Int ->
  m a ->
  m a
retryAction currentErr currentRetryCount maxRetries baseCoefficient action = do
  logWarning $ "Error calling " <> showBaseUrlText currentErr.baseUrl <> ": " <> show currentErr.clientError
  logWarning $ "Retrying attempt " <> show currentRetryCount <> " calling " <> showBaseUrlText currentErr.baseUrl
  Metrics.addUrlCallRetries currentErr.baseUrl currentRetryCount
  catchConnectionErrors action $ \err -> do
    if currentRetryCount < maxRetries
      then do
        liftIO $ threadDelaySec $ Seconds $ baseCoefficient * (2 ^ currentRetryCount)
        retryAction err (currentRetryCount + 1) maxRetries baseCoefficient action
      else do
        logError $ "Maximum of retrying attempts is reached calling " <> showBaseUrlText err.baseUrl
        Metrics.addUrlCallRetryFailures currentErr.baseUrl
        throwError err

withRetryConfig ::
  ( MonadCatch m,
    MonadReader r m,
    MonadIO m,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  RetryCfg ->
  m a ->
  m a
withRetryConfig retryConfig action = do
  let maxRetries = retryConfig.maxRetries
      baseCoefficient = retryConfig.baseCoefficient

  catchConnectionErrors action $ \err -> do
    if maxRetries > 0
      then retryAction err 1 maxRetries baseCoefficient action
      else do
        Metrics.addUrlCallRetryFailures err.baseUrl
        throwError err

withShortRetry ::
  ( MonadCatch m,
    MonadReader r m,
    MonadIO m,
    HasShortDurationRetryCfg r c,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  m a ->
  m a
withShortRetry action = do
  retryConfig <- asks (.shortDurationRetryCfg)
  withRetryConfig retryConfig action

withLongRetry ::
  ( MonadCatch m,
    MonadReader r m,
    MonadIO m,
    HasLongDurationRetryCfg r c,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  m a ->
  m a
withLongRetry action = do
  retryConfig <- asks (.longDurationRetryCfg)
  withRetryConfig retryConfig action

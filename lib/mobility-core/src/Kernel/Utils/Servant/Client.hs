{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Servant.Client where

import qualified Data.Aeson as A
import qualified Data.HashMap as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, notElem)
import qualified EulerHS.Types as ET
import GHC.Records.Extra (HasField)
import Kernel.Prelude as KP
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.CallAPIError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.BaseUrl
import Kernel.Utils.Text
import Kernel.Utils.Time
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Types (status404)
import qualified Network.Wai as Wai
import Network.Wai.Application.Static (staticApp)
import qualified Servant
import Servant.Client.Core
import qualified Text.Regex as TR
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types (StaticSettings (..))

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

type CallAPI' m api res res' =
  ( HasCallStack,
    Metrics.CoreMetrics m,
    SanitizedUrl api,
    MonadFlow m,
    ToJSON res
  ) =>
  BaseUrl ->
  ET.EulerClient res ->
  Text ->
  Proxy api ->
  m res'

type CallAPI m api res = CallAPI' m api res res

callAPI ::
  CallAPI' m api res (Either ClientError res)
callAPI = callAPI' Nothing

-- Why do we call L.callAPI' (Just "default") instead of L.callAPI' Nothing?
callAPI' ::
  Maybe ET.ManagerSelector ->
  CallAPI' m api res (Either ClientError res)
callAPI' mbManagerSelector baseUrl eulerClient desc api =
  withLogTag "callAPI" $ do
    let managerSelector = fromMaybe defaultHttpManager mbManagerSelector
    logDebug $ "Sanitized URL is " <> buildSanitizedUrl
    res <-
      measuringDuration (Metrics.addRequestLatency buildSanitizedUrl desc) $
        L.callAPI' (Just managerSelector) baseUrl eulerClient
    case res of
      Right r -> logDebug $ "Ok response: " <> truncateText (decodeUtf8 (A.encode r))
      Left err -> logDebug $ "Error occured during client call: " <> show err
    return res
  where
    buildSanitizedUrl = do
      let url = T.split (== '/') $ T.pack (baseUrlPath baseUrl)
          urlPath = if listToMaybe url == Just "" then drop 1 url else url
      let req = Wai.defaultRequest
          baseRequest = req {Wai.pathInfo = urlPath}
      fromMaybe (removeUUID $ showBaseUrlText baseUrl) (getSanitizedUrl api baseRequest)

    removeUUID url = T.pack $ TR.subRegex (TR.mkRegex "[0-9a-z]{8}-([0-9a-z]{4}-){3}[0-9a-z]{12}") (T.unpack url) ":id"

callApiExtractingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err
  ) =>
  Maybe ET.ManagerSelector ->
  CallAPI' m api a (Either (CallAPIError err) a)
callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc api =
  callAPI' mbManagerSelector baseUrl eulerClient desc api
    <&> extractApiError

callApiUnwrappingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err,
    IsHTTPException exc
  ) =>
  (err -> exc) ->
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  Maybe (HM.Map Text Text) ->
  CallAPI m api a
callApiUnwrappingApiError toAPIException mbManagerSelector errorCodeMb aclEndPointHashMap baseUrl eulerClient desc api = do
  case aclEndPointHashMap of
    Nothing ->
      do
        callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc api
        >>= unwrapEitherCallAPIError errorCodeMb baseUrl toAPIException
    Just aclEndPointMap -> do
      newBaseUrl <- KP.parseBaseUrl $ fromMaybe (KP.showBaseUrl baseUrl) $ HM.lookup (KP.showBaseUrl baseUrl) aclEndPointMap
      callApiExtractingApiError mbManagerSelector newBaseUrl eulerClient desc api
        >>= unwrapEitherCallAPIError errorCodeMb newBaseUrl toAPIException

-- Note on change:-
-- As "ManagerSelector" is now a newtype wrapper around "Text" in new Euler-hs
defaultHttpManager :: ET.ManagerSelector
defaultHttpManager = ET.ManagerSelector "default"

setResponseTimeout :: Int -> Http.ManagerSettings -> Http.ManagerSettings
setResponseTimeout timeout settings =
  settings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

createManagers ::
  ( MonadReader r m,
    HasHttpClientOptions r c,
    MonadFlow m
  ) =>
  HashMap Text Http.ManagerSettings ->
  m (HashMap Text Http.Manager)
createManagers managerSettings = do
  timeout <- asks (.httpClientOptions.timeoutMs)
  liftIO $ managersFromManagersSettings timeout managerSettings

createManagersWithTimeout ::
  ( MonadReader r m,
    HasHttpClientOptions r c,
    MonadFlow m
  ) =>
  HashMap Text Http.ManagerSettings ->
  Maybe Int ->
  m (HashMap Text Http.Manager)
createManagersWithTimeout managerSettings Nothing = createManagers managerSettings
createManagersWithTimeout managerSettings (Just timeout) = liftIO $ managersFromManagersSettings timeout managerSettings

managersFromManagersSettings ::
  Int ->
  HashMap Text Http.ManagerSettings ->
  IO (HashMap Text Http.Manager)
managersFromManagersSettings timeout =
  mapM Http.newManager
    . fmap (setResponseTimeout timeout)
    . HMS.insert defaultHttpManagerString Http.tlsManagerSettings
  where
    extractDefaultManagerString (ET.ManagerSelector x) = x
    defaultHttpManagerString = extractDefaultManagerString defaultHttpManager

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

serveDirectoryWebApp :: FilePath -> Servant.ServerT Servant.Raw m
serveDirectoryWebApp = serveDirectoryWith' . settings . defaultWebAppSettings
  where
    staticApp' :: StaticSettings -> Wai.Application
    staticApp' _ req sendResponse
      | Wai.requestMethod req `notElem` ["GET", "HEAD"] =
        sendError404 sendResponse
    staticApp' set req sendResponse = staticApp set req sendResponse
    serveDirectoryWith' = Servant.Tagged . staticApp'
    settings StaticSettings {..} = StaticSettings {ss404Handler = Just $ \_ sendResponse -> sendError404 sendResponse, ..}
    sendError404 sendResponse =
      sendResponse $
        Wai.responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Not found"

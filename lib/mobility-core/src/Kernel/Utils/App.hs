{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Kernel.Utils.App
  ( Shutdown,
    handleLeftIO,
    handleLeft,
    handleShutdown,
    logRequestAndResponse,
    withModifiedEnv,
    hashBodyForSignature,
    getPodName,
    lookupDeploymentVersion,
    supportProxyAuthorization,
    logRequestAndResponseGeneric,
    withModifiedEnvGeneric,
    withModifiedEnv',
    logRequestAndResponse',
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.Default.Class
import qualified Data.HashMap.Internal as HM
import Data.List (lookup)
import Data.String.Conversions
import qualified Data.Text as T (pack)
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (unpack)
import qualified EulerHS.Runtime as R
import Kernel.Beam.ART.ARTUtils
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Storage.Hedis
import Kernel.Tools.Logging
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion (..))
import Kernel.Tools.Metrics.CoreMetrics.Types (HasCoreMetrics)
import Kernel.Types.App
import Kernel.Types.Flow
import Kernel.Utils.Common
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.IOLogging (HasLog, appendLogTag, updateLogLevel)
import Kernel.Utils.Shutdown
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Network.HTTP.Types (Method, RequestHeaders)
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai as Wai
import Network.Wai.Internal
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import qualified Text.Regex as TR

data RequestInfo = RequestInfo
  { requestMethod :: Method,
    rawPathInfo :: ByteString,
    rawQueryString :: ByteString,
    requestHeaders :: RequestHeaders
  }
  deriving (Show)

data ResponseInfo = ResponseInfo
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: [(Text, Text)]
  }
  deriving (Show)

handleLeftIO :: forall a b. (Show a) => ExitCode -> Text -> Either a b -> IO b
handleLeftIO exitCode msg =
  fromEitherM' \err -> do
    print (msg <> show err)
    liftIO $ exitWith exitCode

handleLeft :: forall a b m. (Show a, Log m, MonadIO m) => ExitCode -> Text -> Either a b -> m b
handleLeft exitCode msg =
  fromEitherM' \err -> do
    logError (msg <> show err)
    liftIO $ exitWith exitCode

hashBodyForSignature :: Application -> Application
hashBodyForSignature f req respF = do
  req' <-
    if anyAuthHeaders
      then do
        body <- strictRequestBody req <&> LB.toStrict
        mvar <- newMVar body
        let requestHeaders =
              ( HttpSig.bodyHashHeader,
                Base64.encode $ BA.convert $ HttpSig.becknSignatureHash body
              ) :
              Wai.requestHeaders req
        pure req {requestBody = mkRequestBody mvar, requestHeaders}
      else pure req
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe B.empty
    headers = map fst $ Wai.requestHeaders req
    anyAuthHeaders =
      any
        (`elem` headers)
        [ "Authorization",
          "Proxy-Authorization",
          "X-Gateway-Authorization",
          "Signature"
        ]

-- TODO: remove when Proxy-Authorization becomes deprecated
supportProxyAuthorization :: Application -> Application
supportProxyAuthorization f =
  f . modifyRequestHeaders \headers ->
    case lookup "X-Gateway-Authorization" headers of
      Nothing ->
        -- check for proxy-auth only if there's no x-gateway-auth
        case lookup "Proxy-Authorization" headers of
          Just h -> ("X-Gateway-Authorization", h) : headers
          Nothing -> headers
      Just _ -> headers

modifyRequestHeaders :: (RequestHeaders -> RequestHeaders) -> Request -> Request
modifyRequestHeaders f req = req {Wai.requestHeaders = f (Wai.requestHeaders req)}

logRequestAndResponse :: HasLog f => EnvR f -> Application -> Application
logRequestAndResponse (EnvR flowRt appEnv) =
  logRequestAndResponseGeneric logInfoIO
  where
    logInfoIO tag info = runFlowR flowRt appEnv $ logTagInfo tag info

logRequestAndResponse' :: (HasARTFlow f) => EnvR f -> Application -> Application
logRequestAndResponse' (EnvR flowRt appEnv) =
  logRequestAndResponseGeneric' appEnv logInfoIO
  where
    logInfoIO tag info = runFlowR flowRt appEnv $ logTagInfo tag info

logRequestAndResponseGeneric' :: HasARTFlow f => f -> (Text -> Text -> IO ()) -> Application -> Application
logRequestAndResponseGeneric' appEnv logInfoIO f req respF = do
  if not appEnv.shouldLogRequestId
    then logRequestAndResponseGeneric logInfoIO f req respF
    else do
      timestamp <- getCurrentTime
      body <- Wai.consumeRequestBodyStrict req
      let requestMethod = Wai.requestMethod req
          rawPathInfo = Wai.rawPathInfo req
          rawQueryString = Wai.rawQueryString req
          requestHeaders = Wai.requestHeaders req

      let request = RequestInfo' {body = T.pack $ show body, requestHeaders = show requestHeaders, requestMethod = show requestMethod, rawPathInfo = show rawPathInfo, rawQueryString = show rawQueryString}
      called :: IORef Int <- newIORef 0
      let returnBody = do
            calledTimes <- readIORef called
            modifyIORef called (+ 1)
            pure $
              if calledTimes > 0
                then B.empty
                else LB.toStrict body
      f (req {requestBody = returnBody}) (loggedRespF timestamp request)
  where
    toRequestInfo Request {..} = RequestInfo {..}
    toResponseInfo resp =
      let (status, headers, _) = responseToStream resp
          code = HTTP.statusCode status
          decodeHeader = bimap (decodeUtf8 . CI.original) decodeUtf8
          respInfo =
            ResponseInfo
              { statusCode = code,
                statusMessage = decodeUtf8 $ HTTP.statusMessage status,
                headers = decodeHeader <$> headers
              }
       in if code >= 300 then show respInfo else "Successful response"

    getBody resp = case resp of
      (ResponseBuilder _ _ builder) -> decodeUtf8 $ toLazyByteString builder
      _ -> "No Body Found for requestId : " <> fromMaybe "" (appEnv.requestId)

    loggedRespF timestamp request resp = do
      let respLogText = toResponseInfo resp
      logInfoIO "Request&Response" $ "Request: " <> show (toRequestInfo req) <> " || Response: " <> respLogText
      when appEnv.shouldLogRequestId $ do
        let artData = def {requestId = fromMaybe "" appEnv.requestId, request = Just request, response = Just $ getBody resp, timestamp = Just timestamp}
        void $ forkIO $ pushToKafka appEnv.kafkaProducerForART (A.encode artData) "ART-Logs" (fromMaybe "" appEnv.requestId)
      respF resp

logRequestAndResponseGeneric :: (Text -> Text -> IO ()) -> Application -> Application
logRequestAndResponseGeneric logInfoIO f req respF =
  f req loggedRespF
  where
    toRequestInfo Request {..} = RequestInfo {..}
    toResponseInfo resp =
      let (status, headers, _) = responseToStream resp
          code = HTTP.statusCode status
          decodeHeader = bimap (decodeUtf8 . CI.original) decodeUtf8
          respInfo =
            ResponseInfo
              { statusCode = code,
                statusMessage = decodeUtf8 $ HTTP.statusMessage status,
                headers = decodeHeader <$> headers
              }
       in if code >= 300 then show respInfo else "Successful response"
    loggedRespF resp = do
      let respLogText = toResponseInfo resp
      logInfoIO "Request&Response" $ "Request: " <> show (toRequestInfo req) <> " || Response: " <> respLogText
      respF resp

withModifiedEnv :: HasLog f => (EnvR f -> Application) -> EnvR f -> Application
withModifiedEnv = withModifiedEnvFn $ \_ env requestId -> do
  let appEnv = env.appEnv
      updLogEnv = appendLogTag requestId appEnv.loggerEnv
  newFlowRt <- L.updateLoggerContext (L.appendLogContext requestId) $ flowRuntime env
  newOptionsLocal <- newMVar mempty
  pure $
    env{appEnv = appEnv{loggerEnv = updLogEnv},
        flowRuntime = newFlowRt {R._optionsLocal = newOptionsLocal}
       }

withModifiedEnv' :: (HasARTFlow f, HasCoreMetrics f, HasField "esqDBEnv" f EsqDBEnv, HedisFlowEnv f, HasCacheConfig f, HasSchemaName BeamSC.SystemConfigsT, HasCacConfig f) => (EnvR f -> Application) -> EnvR f -> Application
withModifiedEnv' = withModifiedEnvFn $ \req env requestId -> do
  let sanitizedUrl = removeUUIDs . cs $ Wai.rawPathInfo req
  mbDynamicLogLevelConfig <- runFlowR env.flowRuntime env.appEnv $ getDynamicLogLevelConfig
  modifyEnvR env (HM.lookup sanitizedUrl =<< mbDynamicLogLevelConfig) requestId
  where
    removeUUIDs path = do
      T.pack $ TR.subRegex (TR.mkRegex "[0-9a-z]{8}-([0-9a-z]{4}-){3}[0-9a-z]{12}") path ":id"
    modifyEnvR env mbLogLevel requestId = do
      let appEnv = env.appEnv
          updLogEnv = appendLogTag requestId appEnv.loggerEnv
          updLogEnv' = updateLogLevel mbLogLevel updLogEnv
      let requestId' = bool Nothing (Just requestId) appEnv.shouldLogRequestId
      newFlowRt <- L.updateLoggerContext (L.appendLogContext requestId) $ flowRuntime env
      newOptionsLocal <- newMVar mempty
      pure $
        env{appEnv = appEnv{loggerEnv = updLogEnv', requestId = requestId'},
            flowRuntime = newFlowRt {R._optionsLocal = newOptionsLocal}
           }

withModifiedEnvFn :: HasLog f => (Wai.Request -> EnvR f -> Text -> IO (EnvR f)) -> (EnvR f -> Application) -> EnvR f -> Application
withModifiedEnvFn modifierFn f env = \req resp -> do
  requestId <- getRequestId $ Wai.requestHeaders req
  modifiedEnv <- modifierFn req env requestId
  let app = f modifiedEnv
  app req resp
  where
    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> decodeUtf8 val)
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom

withModifiedEnvGeneric :: HasLog env => (env -> Application) -> env -> Application
withModifiedEnvGeneric f env = \req resp -> do
  requestId <- getRequestId $ Wai.requestHeaders req
  let modifiedEnv = modifyEnv requestId
  let app = f modifiedEnv
  app req resp
  where
    modifyEnv requestId = do
      let appEnv = env
          updLogEnv = appendLogTag requestId appEnv.loggerEnv
      env{loggerEnv = updLogEnv
         }
    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> decodeUtf8 val)
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom

getPodName :: IO (Maybe Text)
getPodName = fmap T.pack <$> lookupEnv "POD_NAME"

lookupDeploymentVersion :: IO DeploymentVersion
lookupDeploymentVersion = DeploymentVersion . T.pack . fromMaybe "DEV" <$> lookupEnv "DEPLOYMENT_VERSION"

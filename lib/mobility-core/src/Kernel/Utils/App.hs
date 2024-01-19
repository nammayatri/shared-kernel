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
  )
where

import Control.Lens (lens)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.Generics.Product (HasField' (..))
import Data.List (lookup)
import qualified Data.Text as T (pack)
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (unpack)
import qualified EulerHS.Runtime as R
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion (..))
import Kernel.Types.App
import Kernel.Types.Flow
import Kernel.Utils.Common
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.IOLogging (HasLog, appendLogTag)
import Kernel.Utils.Shutdown
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Network.HTTP.Types (Method, RequestHeaders)
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai as Wai
import Network.Wai.Internal
import System.Environment (lookupEnv)
import System.Exit (ExitCode)

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
withModifiedEnv f env = \req resp -> do
  requestId <- getRequestId $ Wai.requestHeaders req
  modifiedEnv <- modifyEnvR requestId
  let app = f modifiedEnv
  app req resp
  where
    modifyEnvR requestId = do
      let appEnv = env ^. #appEnv
          updLogEnv = appendLogTag requestId (appEnv ^. #loggerEnv)
      newFlowRt <- L.updateLoggerContext (L.appendLogContext requestId) $ env ^. #flowRuntime
      newOptionsLocal <- newMVar mempty
      pure $
        env
          & #appEnv . field' @"loggerEnv" .~ updLogEnv
          & #flowRuntime .~ (newFlowRt & optionsLocalLens .~ newOptionsLocal)

    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> decodeUtf8 val)
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom

optionsLocalLens :: Lens' R.FlowRuntime (MVar (Map Text Any))
optionsLocalLens = lens getter setter
  where
    getter = R._optionsLocal
    setter fr newVal = fr {R._optionsLocal = newVal}

withModifiedEnvGeneric :: HasLog env => (env -> Application) -> env -> Application
withModifiedEnvGeneric f env = \req resp -> do
  requestId <- getRequestId $ Wai.requestHeaders req
  let modifiedEnv = modifyEnv requestId
  let app = f modifiedEnv
  app req resp
  where
    modifyEnv requestId = do
      let appEnv = env
          updLogEnv = appendLogTag requestId (appEnv ^. #loggerEnv)
      env & field' @"loggerEnv" .~ updLogEnv
    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> decodeUtf8 val)
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom

getPodName :: IO (Maybe Text)
getPodName = fmap T.pack <$> lookupEnv "POD_NAME"

lookupDeploymentVersion :: IO DeploymentVersion
lookupDeploymentVersion = DeploymentVersion . T.pack . fromMaybe "DEV" <$> lookupEnv "DEPLOYMENT_VERSION"

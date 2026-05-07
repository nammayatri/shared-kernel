{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import qualified Kernel.External.MasterCloudForward.API as MFAPI
import qualified Kernel.External.MasterCloudForward.Flow as MF
import qualified Kernel.External.MasterCloudForward.Types as MFT
import Kernel.Prelude hiding (length)
import qualified Kernel.Streaming.Kafka.Producer.Types as Kafka
import qualified Kernel.Tools.Metrics.CoreMetrics.Types as Metrics
import qualified Kernel.Types.Flow as KFlow
import Kernel.Types.Logging (LogLevel (..), LoggerConfig (..))
import qualified Kernel.Utils.IOLogging as IOLogging
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import Servant (JSON, Post, ReqBody, (:>))
import qualified Servant
import qualified Servant.Client as SC
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified Prelude as P

-- ===========================================================================
-- External provider Servant API (the real upstream the typed client targets)
-- ===========================================================================

data EchoReq = EchoReq {msg :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data EchoResp = EchoResp {received :: Text, length :: Int}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

type ExternalAPI = "echo" :> ReqBody '[JSON] EchoReq :> Post '[JSON] EchoResp

externalAPI :: Proxy ExternalAPI
externalAPI = Proxy

externalServer :: IORef Int -> Servant.Server ExternalAPI
externalServer hits = \req -> do
  liftIO $ atomicModifyIORef' hits (\n -> (n + 1, ()))
  pure
    EchoResp
      { received = "echoed: " <> req.msg,
        length = T.length req.msg
      }

externalApp :: IORef Int -> Servant.Application
externalApp hits = Servant.serve externalAPI (externalServer hits)

-- The typed Servant client used by the test (talks to ExternalAPI).
echoClient :: EchoReq -> ET.EulerClient EchoResp
echoClient = ET.client externalAPI

-- ===========================================================================
-- AWS forwarder Servant server (mounts ForwardAPI)
-- ===========================================================================

forwarderServer ::
  IORef Int ->
  Text ->
  Http.Manager ->
  Servant.Server MFAPI.ForwardAPI
forwarderServer hits expectedSecret manager mbSecret envelope = do
  liftIO $ atomicModifyIORef' hits (\n -> (n + 1, ()))
  case mbSecret of
    Just s | s == expectedSecret -> pure ()
    _ -> Servant.throwError Servant.err401 {Servant.errBody = "bad-secret"}
  baseReq <- liftIO $ Http.parseRequest (T.unpack envelope.url)
  bodyBytes <-
    case B64.decode (TE.encodeUtf8 envelope.body) of
      Right bs -> pure bs
      Left err ->
        Servant.throwError
          Servant.err400 {Servant.errBody = LBS.fromStrict (TE.encodeUtf8 (T.pack err))}
  -- Drop hop/transport headers; http-client recomputes them.
  let envelopeHeaders =
        [ (CI.mk (TE.encodeUtf8 name), TE.encodeUtf8 value)
          | (name, value) <- envelope.headers,
            let lname = T.toLower name,
            lname /= "content-length",
            lname /= "host",
            lname /= "transfer-encoding"
        ]
      req =
        baseReq
          { Http.method = TE.encodeUtf8 envelope.method,
            Http.requestBody = Http.RequestBodyBS bodyBytes,
            Http.requestHeaders = envelopeHeaders
          }
  resp <-
    liftIO $
      E.try @E.SomeException (Http.httpLbs req manager) >>= \case
        Left e -> P.error ("forwarder upstream failure: " <> P.show e)
        Right r -> pure r
  let respBodyBs = LBS.toStrict (Http.responseBody resp)
      respHeaders =
        [ (TE.decodeUtf8 (CI.original name), TE.decodeUtf8 value)
          | (name, value) <- Http.responseHeaders resp
        ]
  pure
    MFT.ForwardResponse
      { status = HTTP.statusCode (Http.responseStatus resp),
        headers = respHeaders,
        body = TE.decodeUtf8 (B64.encode respBodyBs)
      }

forwarderApp ::
  IORef Int ->
  Text ->
  Http.Manager ->
  Servant.Application
forwarderApp hits expectedSecret manager =
  Servant.serve MFAPI.forwardAPI (forwarderServer hits expectedSecret manager)

-- ===========================================================================
-- TestEnv: minimal record satisfying every constraint MonadFlow / Log /
-- CoreMetrics / HasMasterCloudForwarder / HasARTFlow imposes on FlowR r.
-- ===========================================================================

data TestEnv = TestEnv
  { masterCloudForwarderUrl :: Maybe BaseUrl,
    masterCloudForwarderSecret :: Maybe Text,
    requestId :: Maybe Text,
    sessionId :: Maybe Text,
    loggerEnv :: IOLogging.LoggerEnv,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe Kafka.KafkaProducerTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    version :: Metrics.DeploymentVersion,
    url :: Maybe Text
  }
  deriving (Generic)

-- HasARTFlow is satisfied structurally by the record fields above. We keep
-- @kafkaProducerForART = Nothing@ since 'runThroughMasterCloud' itself never
-- forks; the constraint is only inherited because @MonadFlow@ implies
-- @Forkable (FlowR r)@.
instance MF.HasMasterCloudForwarder TestEnv where
  masterCloudForwarderUrl = (.masterCloudForwarderUrl)
  masterCloudForwarderSecret = (.masterCloudForwarderSecret)

-- ===========================================================================
-- Test harness
-- ===========================================================================

externalPort, forwarderPort :: Int
externalPort = 9001
forwarderPort = 9002

mkBaseUrl :: P.String -> Int -> BaseUrl
mkBaseUrl host port =
  BaseUrl
    { baseUrlScheme = SC.Http,
      baseUrlHost = host,
      baseUrlPort = port,
      baseUrlPath = ""
    }

silentLoggerConfig :: LoggerConfig
silentLoggerConfig =
  LoggerConfig
    { level = ERROR,
      logToFile = False,
      logFilePath = "/tmp/master-cloud-forward-itest.log",
      logToConsole = False,
      logRawSql = False,
      prettyPrinting = False
    }

bootstrapEnv ::
  Maybe BaseUrl ->
  Maybe Text ->
  IOLogging.LoggerEnv ->
  Metrics.CoreMetricsContainer ->
  TestEnv
bootstrapEnv mbFwdUrl mbSecret logger metrics =
  TestEnv
    { masterCloudForwarderUrl = mbFwdUrl,
      masterCloudForwarderSecret = mbSecret,
      requestId = Just "itest-req-id",
      sessionId = Just "itest-session",
      loggerEnv = logger,
      shouldLogRequestId = False,
      kafkaProducerForART = Nothing,
      coreMetrics = metrics,
      version = Metrics.DeploymentVersion "itest-0.0.0",
      url = Nothing
    }

-- A scenario report.
data Outcome = Pass Text | Fail Text Text

reportOutcome :: Outcome -> IO ()
reportOutcome = \case
  Pass label -> P.putStrLn ("PASS " <> T.unpack label)
  Fail label why ->
    P.putStrLn ("FAIL " <> T.unpack label <> ": " <> T.unpack why)

isFail :: Outcome -> Bool
isFail = \case
  Pass _ -> False
  Fail _ _ -> True

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  P.putStrLn "[itest] starting master-cloud-forward integration test"

  fwdHits <- newIORef (0 :: Int)
  externalHits <- newIORef (0 :: Int)
  forwarderManager <- Http.newManager Http.defaultManagerSettings

  let extApp = externalApp externalHits
      runExternal = Warp.run externalPort extApp

  let mkForwarderApp secret = forwarderApp fwdHits secret forwarderManager

  loggerEnv <- IOLogging.prepareLoggerEnv silentLoggerConfig (Just "itest-host")
  metrics <- Metrics.registerCoreMetricsContainer

  -- Spawn external server (always running).
  externalAsync <- Async.async runExternal

  -- Run forwarder per scenario so we can vary the expected secret.
  let withForwarder secret action = do
        Async.withAsync (Warp.run forwarderPort (mkForwarderApp secret)) $ \_ -> do
          threadDelay 200000
          action

  let externalUrl = mkBaseUrl "localhost" externalPort
      forwarderUrl = mkBaseUrl "localhost" forwarderPort

  -- Wait for external to bind.
  threadDelay 200000

  outcomes <-
    R.withFlowRuntime Nothing $ \flowRt0 -> do
      -- runThroughMasterCloud passes the named "default" manager selector to
      -- L.callAPI'. The void runtime doesn't pre-register that name, so we
      -- inject it here. (In production this is done by the app's
      -- 'managersFromManagersSettings' bootstrap.)
      defaultMgr <- Http.newManager Http.defaultManagerSettings
      let flowRt =
            flowRt0
              { R._httpClientManagers =
                  HM.insert "default" defaultMgr (R._httpClientManagers flowRt0)
              }
      -- =================================================================
      -- Scenario A: forward path active
      -- =================================================================
      a <- do
        atomicWriteIORef fwdHits 0
        atomicWriteIORef externalHits 0
        setEnv "RUN_API_IN_MASTER_CLOUD" "True"
        let env =
              bootstrapEnv
                (Just forwarderUrl)
                (Just "test-secret-123")
                loggerEnv
                metrics
        withForwarder "test-secret-123" $ do
          result <-
            KFlow.runFlowR flowRt env $
              MF.runThroughMasterCloud externalUrl (echoClient (EchoReq "hello-from-test")) "echo-test"
          fwdSeen <- readIORef fwdHits
          extSeen <- readIORef externalHits
          let expected =
                EchoResp
                  { received = "echoed: hello-from-test",
                    length = T.length ("hello-from-test" :: Text)
                  }
          pure $ case result of
            Right r
              | r == expected && fwdSeen == 1 && extSeen == 1 ->
                Pass "scenario A (forward path)"
              | r == expected ->
                Fail
                  "scenario A (forward path)"
                  ( "response correct but counters off: forwarder="
                      <> T.pack (P.show fwdSeen)
                      <> " external="
                      <> T.pack (P.show extSeen)
                  )
              | otherwise ->
                Fail "scenario A (forward path)" ("unexpected response: " <> T.pack (P.show r))
            Left err ->
              Fail "scenario A (forward path)" ("client error: " <> T.pack (P.show err))

      -- =================================================================
      -- Scenario B: direct path (forwarder bypassed)
      -- =================================================================
      b <- do
        atomicWriteIORef fwdHits 0
        atomicWriteIORef externalHits 0
        unsetEnv "RUN_API_IN_MASTER_CLOUD"
        let env =
              bootstrapEnv
                (Just forwarderUrl)
                (Just "test-secret-123")
                loggerEnv
                metrics
        -- We still spin up the forwarder, but it should NOT be hit.
        withForwarder "test-secret-123" $ do
          result <-
            KFlow.runFlowR flowRt env $
              MF.runThroughMasterCloud externalUrl (echoClient (EchoReq "hello-from-test")) "echo-test"
          fwdSeen <- readIORef fwdHits
          extSeen <- readIORef externalHits
          let expected =
                EchoResp
                  { received = "echoed: hello-from-test",
                    length = T.length ("hello-from-test" :: Text)
                  }
          pure $ case result of
            Right r
              | r == expected && fwdSeen == 0 && extSeen == 1 ->
                Pass "scenario B (direct path)"
              | r == expected ->
                Fail
                  "scenario B (direct path)"
                  ( "response correct but counters off: forwarder="
                      <> T.pack (P.show fwdSeen)
                      <> " external="
                      <> T.pack (P.show extSeen)
                  )
              | otherwise ->
                Fail "scenario B (direct path)" ("unexpected response: " <> T.pack (P.show r))
            Left err ->
              Fail "scenario B (direct path)" ("client error: " <> T.pack (P.show err))

      -- =================================================================
      -- Scenario C: bad secret -> forwarder returns 401
      -- =================================================================
      c <- do
        atomicWriteIORef fwdHits 0
        atomicWriteIORef externalHits 0
        setEnv "RUN_API_IN_MASTER_CLOUD" "True"
        let env =
              bootstrapEnv
                (Just forwarderUrl)
                (Just "wrong-secret")
                loggerEnv
                metrics
        withForwarder "test-secret-123" $ do
          result <-
            KFlow.runFlowR flowRt env $
              MF.runThroughMasterCloud externalUrl (echoClient (EchoReq "hello-from-test")) "echo-test"
          extSeen <- readIORef externalHits
          pure $ case result of
            Left err ->
              let shown = T.pack (P.show err)
               in if extSeen == 0 && ("401" `T.isInfixOf` shown)
                    then Pass "scenario C (bad secret)"
                    else
                      Fail
                        "scenario C (bad secret)"
                        ( "got Left but expected 401 + zero external hits; ext="
                            <> T.pack (P.show extSeen)
                            <> " err="
                            <> shown
                        )
            Right r ->
              Fail
                "scenario C (bad secret)"
                ("expected Left, got Right " <> T.pack (P.show r))

      pure [a, b, c]

  -- Tear down
  Async.cancel externalAsync
  IOLogging.releaseLoggerEnv loggerEnv

  mapM_ reportOutcome outcomes
  if any isFail outcomes
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess

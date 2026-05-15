{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- Real-world parity test: GET https://jsonplaceholder.typicode.com/todos/1
-- via plain `callAPI` and via `runThroughMasterCloud` (through a local
-- forwarder), then assert both decoded `Todo` values are identical.
module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exc
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import Kernel.External.MasterCloudForward
  ( HasMasterCloudForwarder (..),
    MasterCloudProxyConfig (..),
    forwardEgressApp,
    runThroughMasterCloud,
  )
import Kernel.Prelude
import qualified Kernel.Streaming.Kafka.Producer.Types as Kafka
import qualified Kernel.Tools.Metrics.CoreMetrics.Types as Metrics
import qualified Kernel.Types.Flow as KFlow
import Kernel.Types.Logging (LogLevel (..), LoggerConfig (..))
import qualified Kernel.Utils.IOLogging as IOLogging
import Kernel.Utils.Servant.Client (callAPI)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import qualified Servant.Client as SC
import System.Environment (setEnv)
import System.Exit (ExitCode (..), exitWith)
import qualified Prelude as P

-- Mirrors the real /todos/{id} response shape.
data Todo = Todo
  { userId :: Int,
    id :: Int,
    title :: Text,
    completed :: Bool
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

type TodoAPI = "todos" :> Capture "id" Int :> Get '[JSON] Todo

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

getTodoClient :: Int -> ET.EulerClient Todo
getTodoClient = ET.client todoAPI

-- Minimal AppEnv carrying the forwarder config plus the fields
-- MonadFlow / HasARTFlow / HasCoreMetrics demand on r.
data TestEnv = TestEnv
  { teProxyConfig :: MasterCloudProxyConfig,
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

instance HasMasterCloudForwarder TestEnv where
  masterCloudProxyConfig = teProxyConfig

forwarderPort :: Int
forwarderPort = 9101

mkBaseUrl :: SC.Scheme -> P.String -> Int -> P.String -> BaseUrl
mkBaseUrl scheme host port path =
  BaseUrl
    { baseUrlScheme = scheme,
      baseUrlHost = host,
      baseUrlPort = port,
      baseUrlPath = path
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
  IOLogging.LoggerEnv ->
  Metrics.CoreMetricsContainer ->
  TestEnv
bootstrapEnv logger metrics =
  TestEnv
    { teProxyConfig =
        MasterCloudProxyConfig
          { masterUrl = Just (mkBaseUrl SC.Http "localhost" forwarderPort ""),
            masterSecret = Just "itest-secret"
          },
      requestId = Just "itest-req",
      sessionId = Just "itest-sess",
      loggerEnv = logger,
      shouldLogRequestId = False,
      kafkaProducerForART = Nothing,
      coreMetrics = metrics,
      version = Metrics.DeploymentVersion "itest-0.0.0",
      url = Nothing
    }

-- Wraps forwardEgressApp with a hit counter so we can assert traffic actually
-- traversed it.
countingForwarderApp ::
  IOLogging.LoggerEnv ->
  IORef Int ->
  MasterCloudProxyConfig ->
  Http.Manager ->
  Application
countingForwarderApp logEnv hits cfg mgr req sendResp = do
  modifyIORef' hits (+ 1)
  forwardEgressApp logEnv cfg mgr req sendResp

main :: IO ()
main = do
  P.putStrLn "[itest] starting jsonplaceholder forwarder parity test"

  -- TLS-aware Manager: used by both the local forwarder (to reach typicode)
  -- AND the EulerHS FlowRuntime (for both the direct callAPI path and the
  -- GCP→forwarder hop).
  mgr <- Http.newManager HttpTLS.tlsManagerSettings

  forwarderHits <- newIORef (0 :: Int)
  let fwdCfg =
        MasterCloudProxyConfig
          { masterUrl = Nothing,
            masterSecret = Just "itest-secret"
          }

  result <- Exc.try @Exc.SomeException $
    IOLogging.withLoggerEnv silentLoggerConfig (Just "itest") $ \logger -> do
      forwarderAsync <-
        Async.async . Warp.run forwarderPort $
          countingForwarderApp logger forwarderHits fwdCfg mgr

      -- Let Warp bind.
      liftIO $ threadDelay 1000000

      coreMx <- Metrics.registerCoreMetricsContainer
      R.withFlowRuntime Nothing $ \flowRt0 -> do
        let flowRt = flowRt0 {R._httpClientManagers = HM.insert "default" mgr (R._httpClientManagers flowRt0)}
            env = bootstrapEnv logger coreMx
            todoBaseUrl = mkBaseUrl SC.Https "jsonplaceholder.typicode.com" 443 ""

        -- Path 1: plain callAPI — direct to typicode, baseline.
        rDirect <-
          KFlow.runFlowR flowRt env $
            callAPI todoBaseUrl (getTodoClient 1) "getTodo-direct" todoAPI

        -- Path 2: runThroughMasterCloud — env on, forwarder configured →
        -- request goes through localhost forwarder which replays to typicode.
        setEnv "RUN_API_IN_MASTER_CLOUD" "True"
        rForwarded <-
          KFlow.runFlowR flowRt env $
            runThroughMasterCloud todoBaseUrl (getTodoClient 1) "getTodo-forwarded"

        -- Compare.
        case (rDirect, rForwarded) of
          (Right todoDirect, Right todoFwd) -> do
            P.putStrLn $ "[itest] direct    : " <> show todoDirect
            P.putStrLn $ "[itest] forwarded : " <> show todoFwd
            when (todoDirect /= todoFwd) $
              fail' ("mismatch: direct=" <> show todoDirect <> " forwarded=" <> show todoFwd)
            fHits <- readIORef forwarderHits
            when (fHits /= 1) $
              fail' ("forwarder hits=" <> show fHits <> ", expected 1")
            P.putStrLn "PASS — direct and forwarded responses are byte-for-byte identical"
            P.putStrLn $ "      forwarder hit count: " <> show fHits
          (Left e, _) ->
            fail' ("direct callAPI failed: " <> show e)
          (_, Left e) ->
            fail' ("forwarded call failed: " <> show e)

        Async.cancel forwarderAsync

  case result of
    Left e -> do
      P.putStrLn $ "[itest] EXCEPTION: " <> P.show e
      exitWith (ExitFailure 1)
    Right _ -> exitWith ExitSuccess
  where
    fail' reason = do
      P.putStrLn $ T.unpack ("FAIL: " <> T.pack reason)
      exitWith (ExitFailure 1)

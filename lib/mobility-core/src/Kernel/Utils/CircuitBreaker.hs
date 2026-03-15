{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Circuit breaker for external service calls.
--
-- Prevents cascading failures by tracking consecutive errors and short-circuiting
-- calls to unhealthy services. Uses a simple three-state machine:
--
--   * __Closed__ (normal) -- requests flow through. After @failureThreshold@
--     consecutive failures the breaker moves to /Open/.
--   * __Open__ -- all requests are rejected immediately with
--     'CircuitBreakerOpen'. After @cooldownSeconds@ the breaker moves to /HalfOpen/.
--   * __HalfOpen__ -- a single probe request is allowed through.
--     If it succeeds the breaker resets to /Closed/; if it fails the breaker
--     returns to /Open/.
--
-- Usage:
--
-- @
-- -- Get or create a breaker for a named service:
-- cb <- getOrCreateCircuitBreaker "GoogleMaps" (defaultCircuitBreakerConfig "GoogleMaps")
-- withCircuitBreaker cb $ callGoogleMapsAPI ...
-- @
module Kernel.Utils.CircuitBreaker
  ( CircuitBreakerConfig (..),
    CircuitBreaker,
    CircuitBreakerState (..),
    defaultCircuitBreakerConfig,
    newCircuitBreaker,
    withCircuitBreaker,
    getOrCreateCircuitBreaker,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Logging (Log, logError, logWarning)
import System.IO.Unsafe (unsafePerformIO)

-- | Tuning knobs for the breaker.
data CircuitBreakerConfig = CircuitBreakerConfig
  { -- | How many consecutive failures before opening the circuit.
    failureThreshold :: Int,
    -- | Seconds to wait in /Open/ before trying a probe (half-open).
    cooldownSeconds :: Int,
    -- | Human-readable name used in log messages and error text.
    serviceName :: Text
  }
  deriving (Show)

-- | Sensible defaults: open after 5 failures, 30 s cooldown.
defaultCircuitBreakerConfig :: Text -> CircuitBreakerConfig
defaultCircuitBreakerConfig name =
  CircuitBreakerConfig
    { failureThreshold = 5,
      cooldownSeconds = 30,
      serviceName = name
    }

-- | Internal mutable state shared across all callers.
data CircuitBreakerState
  = CBClosed
      { consecutiveFailures :: !Int
      }
  | CBOpen
      { openedAt :: !UTCTime
      }
  | CBHalfOpen
  deriving (Show)

-- | Opaque handle -- create one per external service endpoint and share it.
data CircuitBreaker = CircuitBreaker
  { config :: CircuitBreakerConfig,
    stateVar :: TVar CircuitBreakerState
  }

-- | Create a new breaker in the /Closed/ state.
newCircuitBreaker :: MonadIO m => CircuitBreakerConfig -> m CircuitBreaker
newCircuitBreaker cfg = do
  sv <- liftIO $ newTVarIO (CBClosed 0)
  pure $ CircuitBreaker cfg sv

-- ---------------------------------------------------------------------------
-- Global breaker registry (process-wide singleton)
-- ---------------------------------------------------------------------------

-- | Global registry keyed by service name. Uses 'unsafePerformIO' which is safe
-- here because the IORef is only created once and all mutations go through
-- 'atomicModifyIORef''.
{-# NOINLINE circuitBreakerRegistry #-}
circuitBreakerRegistry :: IORef (Map.Map Text CircuitBreaker)
circuitBreakerRegistry = unsafePerformIO $ newIORef Map.empty

-- | Look up a breaker by name; create one with the given config if absent.
-- Thread-safe via 'atomicModifyIORef''.
getOrCreateCircuitBreaker :: MonadIO m => Text -> CircuitBreakerConfig -> m CircuitBreaker
getOrCreateCircuitBreaker name cfg = liftIO $ do
  existing <- readIORef circuitBreakerRegistry
  case Map.lookup name existing of
    Just cb -> pure cb
    Nothing -> do
      cb <- newCircuitBreaker cfg
      atomicModifyIORef' circuitBreakerRegistry $ \m ->
        case Map.lookup name m of
          Just existingCb -> (m, existingCb) -- another thread beat us
          Nothing -> (Map.insert name cb m, cb)

-- ---------------------------------------------------------------------------
-- Core logic
-- ---------------------------------------------------------------------------

-- | Run @action@ through the circuit breaker.
--
-- If the circuit is open and the cooldown has not elapsed the call is rejected
-- immediately with an 'InternalError'. Otherwise the action is executed and its
-- outcome (success / exception) updates the breaker state.
withCircuitBreaker ::
  ( MonadIO m,
    MonadCatch m,
    Log m
  ) =>
  CircuitBreaker ->
  m a ->
  m a
withCircuitBreaker cb action = do
  allowed <- liftIO $ atomically $ do
    st <- readTVar (cb.stateVar)
    case st of
      CBClosed _ -> pure True
      CBOpen _ -> pure False -- will check cooldown outside STM (needs IO for time)
      CBHalfOpen -> pure True -- probe request allowed
  if allowed
    then executeAndRecord cb action
    else do
      -- Check if cooldown has elapsed
      now <- liftIO getCurrentTime
      shouldProbe <- liftIO $ atomically $ do
        st <- readTVar (cb.stateVar)
        case st of
          CBOpen openTime ->
            if diffUTCTime now openTime >= fromIntegral cb.config.cooldownSeconds
              then do
                writeTVar (cb.stateVar) CBHalfOpen
                pure True
              else pure False
          _ -> pure True -- state changed concurrently, allow
      if shouldProbe
        then executeAndRecord cb action
        else do
          logWarning $ "Circuit breaker OPEN for " <> cb.config.serviceName <> " - rejecting request"
          throwError $ InternalError $ "Circuit breaker open for service: " <> cb.config.serviceName

executeAndRecord ::
  ( MonadIO m,
    MonadCatch m,
    Log m
  ) =>
  CircuitBreaker ->
  m a ->
  m a
executeAndRecord cb action = do
  result <- try action
  case result of
    Right val -> do
      -- Success -- reset to Closed
      liftIO $ atomically $ writeTVar (cb.stateVar) (CBClosed 0)
      pure val
    Left (err :: SomeException) -> do
      now <- liftIO getCurrentTime
      liftIO $ atomically $ do
        st <- readTVar (cb.stateVar)
        case st of
          CBClosed failures ->
            let newFailures = failures + 1
             in if newFailures >= cb.config.failureThreshold
                  then writeTVar (cb.stateVar) (CBOpen now)
                  else writeTVar (cb.stateVar) (CBClosed newFailures)
          CBHalfOpen ->
            -- Probe failed -- reopen
            writeTVar (cb.stateVar) (CBOpen now)
          CBOpen _ ->
            -- Should not normally happen, but keep it open
            pure ()
      logError $ "Circuit breaker recorded failure for " <> cb.config.serviceName <> ": " <> show err
      throwM err

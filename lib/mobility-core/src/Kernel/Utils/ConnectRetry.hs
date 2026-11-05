module Kernel.Utils.ConnectRetry
  ( ConnectRetryPolicy (..),
    getConnectRetryPolicy,
    withConnectRetry,
    withConnectRetryPolicy,
    backoffMicros,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower)
import Data.Time (getCurrentTime)
import Kernel.Prelude
import System.Environment (lookupEnv)

data ConnectRetryPolicy = ConnectRetryPolicy
  { maxAttempts :: Int,
    delayMicros :: Int,
    maxDelayMicros :: Int
  }
  deriving (Show, Eq)

getConnectRetryPolicy :: IO (Maybe ConnectRetryPolicy)
getConnectRetryPolicy = do
  enabled <- maybe False ((== "true") . map toLower) <$> lookupEnv "CONNECT_RETRY_ENABLED"
  if enabled
    then do
      attempts <- fromMaybe 5 . (readMaybe =<<) <$> lookupEnv "CONNECT_RETRY_MAX_ATTEMPTS"
      delayMs <- fromMaybe 2000 . (readMaybe =<<) <$> lookupEnv "CONNECT_RETRY_DELAY_MS"
      maxDelayMs <- fromMaybe 30000 . (readMaybe =<<) <$> lookupEnv "CONNECT_RETRY_MAX_DELAY_MS"
      pure $
        Just
          ConnectRetryPolicy
            { maxAttempts = max 1 attempts,
              delayMicros = max 0 delayMs * 1000,
              maxDelayMicros = max 0 maxDelayMs * 1000
            }
    else pure Nothing

withConnectRetry :: String -> IO a -> IO a
withConnectRetry label action = do
  mbPolicy <- getConnectRetryPolicy
  maybe action (\policy -> withConnectRetryPolicy policy label action) mbPolicy

withConnectRetryPolicy :: ConnectRetryPolicy -> String -> IO a -> IO a
withConnectRetryPolicy policy label action = go 1
  where
    go attempt =
      try action >>= \case
        Right result -> pure result
        Left (e :: SomeException)
          | isAsync e -> throwIO e
          | attempt >= policy.maxAttempts -> do
            logRetryEvent "ERROR" "connect_failed" label attempt policy.maxAttempts e
            throwIO e
          | otherwise -> do
            logRetryEvent "WARNING" "connect_retry" label attempt policy.maxAttempts e
            threadDelay (backoffMicros policy attempt)
            go (attempt + 1)
    isAsync e = isJust (fromException e :: Maybe SomeAsyncException)

backoffMicros :: ConnectRetryPolicy -> Int -> Int
backoffMicros policy attempt = min policy.maxDelayMicros (policy.delayMicros * 2 ^ min 20 (attempt - 1))

logRetryEvent :: String -> String -> String -> Int -> Int -> SomeException -> IO ()
logRetryEvent lvl event label attempt maxAttempts e = do
  now <- getCurrentTime
  BL.putStrLn . A.encode $
    A.object
      [ "timestamp" A..= (show now :: String),
        "lvl" A..= (lvl :: String),
        "tag" A..= ("CONNECT_RETRY" :: String),
        "msg"
          A..= A.object
            [ "event" A..= (event :: String),
              "target" A..= (label :: String),
              "attempt" A..= (attempt :: Int),
              "max_attempts" A..= (maxAttempts :: Int),
              "error" A..= (show e :: String)
            ]
      ]

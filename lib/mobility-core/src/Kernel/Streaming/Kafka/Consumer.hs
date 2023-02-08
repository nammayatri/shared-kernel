module Kernel.Streaming.Kafka.Consumer
  ( Kernel.Streaming.Kafka.Consumer.receiveMessage,
    listenForMessages,
  )
where

import Kernel.Prelude
import Kernel.Streaming.Kafka.Consumer.Types as ConsTypes
import qualified Kernel.Streaming.MonadConsumer as MonadCons
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Logging
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Kafka.Consumer as KafkaCons

receiveMessage :: (MonadIO m, Log m, MonadThrow m, FromJSON a) => KafkaConsumerTools a -> m (Maybe a)
receiveMessage kafkaConsumerTools = withLogTag "KafkaConsumer" $ do
  let timeout = kafkaConsumerTools.kafkaConsumerCfg.timeoutMilliseconds
  etrMsg <- pollMessage kafkaConsumerTools.consumer (Timeout timeout)
  case etrMsg of
    Left err -> handleResponseError err
    Right res -> do
      mbErr <- commitAllOffsets OffsetCommit kafkaConsumerTools.consumer
      whenJust mbErr $ \err -> logError $ "Unable to commit offsets: " <> show err
      crValue res >>= A.decode . LBS.fromStrict
        & fromMaybeM KafkaUnableToParseValue
        <&> Just
  where
    handleResponseError err =
      case err of
        KafkaResponseError RdKafkaRespErrTimedOut -> do
          logInfo "No messages to consume."
          return Nothing
        _ -> throwError $ KafkaUnableToConsumeMessage err

listenForMessages ::
  ( MonadCons.MonadConsumer a m,
    MonadIO m,
    MonadCatch m,
    Log m,
    MonadThrow m
  ) =>
  m Bool ->
  (a -> m ()) ->
  m ()
listenForMessages isRunning handleMessage = whileM isRunning $ do
  etrRes <- try @_ @SomeException MonadCons.receiveMessage
  case etrRes of
    Left err -> logInfo $ "Message was not received: " <> show err
    Right mbRes -> whenJust mbRes handleMessage

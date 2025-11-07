{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Consumer
  ( Kernel.Streaming.Kafka.Consumer.receiveMessage,
    listenForMessages,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Kafka.Consumer as KafkaCons
import Kernel.Prelude
import Kernel.Streaming.Kafka.Consumer.Types as ConsTypes
import qualified Kernel.Streaming.MonadConsumer as MonadCons
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Logging

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
    MonadThrow m,
    TryException m
  ) =>
  m Bool ->
  (a -> m ()) ->
  m ()
listenForMessages isRunning handleMessage = whileM isRunning $ do
  etrRes <- withTryCatch "listenForMessages" MonadCons.receiveMessage
  case etrRes of
    Left err -> logInfo $ "Message was not received: " <> show err
    Right mbRes -> whenJust mbRes handleMessage

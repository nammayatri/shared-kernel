module Kernel.Utils.InternalAPICallLogging where

import Data.Aeson
import Data.Text
import Kernel.Prelude
import qualified Kernel.Streaming.Kafka.Producer as KafkaProd
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import qualified Kernel.Utils.Text as KUT
import System.Environment (lookupEnv)

data Source = BPP | BAP
  deriving (Generic, Read, Show, ToJSON, FromJSON)

data InternalAPICallLog = InternalAPICallLog
  { id :: Id InternalAPICallLog,
    apiName :: Text,
    source :: Text,
    entityId :: Maybe Text,
    requestPayload :: Maybe Text,
    responsePayload :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, Read, Show)

pushInternalApiCallDataToKafka :: (MonadFlow m, MonadReader r m, ToJSON req', ToJSON res', HasKafkaProducer r) => Text -> Text -> Maybe Text -> Maybe req' -> res' -> m ()
pushInternalApiCallDataToKafka apiName svcProvider entityId req res = do
  let res' = KUT.encodeToText res
  kafkaPushEnabled <- liftIO $ fromMaybe True . (readMaybe =<<) <$> lookupEnv "ENABLE_BECKN_LOGS_KAFKA_PUSH"
  when kafkaPushEnabled $ pushInternalApiCallDataToKafkaWithTextEncodedResp apiName svcProvider entityId req res'

pushInternalApiCallDataToKafkaWithTextEncodedResp :: forall m r req'. (MonadFlow m, MonadReader r m, ToJSON req', HasKafkaProducer r) => Text -> Text -> Maybe Text -> Maybe req' -> Text -> m ()
pushInternalApiCallDataToKafkaWithTextEncodedResp apiName source entityId req res = do
  KafkaProd.produceMessage ("Internal-API-Call-Logs", Nothing) =<< buildInternalAPICallLog
  where
    buildInternalAPICallLog = do
      id <- generateGUID
      timestamp <- getCurrentTime
      return $
        InternalAPICallLog
          { requestPayload = KUT.encodeToText <$> req,
            responsePayload = res,
            createdAt = timestamp,
            updatedAt = timestamp,
            ..
          }

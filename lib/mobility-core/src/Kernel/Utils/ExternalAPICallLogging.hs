module Kernel.Utils.ExternalAPICallLogging where

import Data.Aeson
import Data.Either
import Data.Text
import Kernel.Prelude
import qualified Kernel.Streaming.Kafka.Producer as KafkaProd
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import qualified Kernel.Utils.Text as KUT
import System.Environment (lookupEnv)

data ExternalAPICallLog = ExternalAPICallLog
  { id :: Id ExternalAPICallLog,
    apiName :: Text,
    svcProvider :: Text,
    entityId :: Maybe Text,
    requestPayload :: Maybe Text,
    responsePayload :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, Read, Show)

pushExternalApiCallDataToKafka :: (MonadFlow m, MonadReader r m, ToJSON req', ToJSON err, ToJSON res', HasKafkaProducer r) => Text -> Text -> Maybe Text -> Maybe req' -> Either err res' -> m ()
pushExternalApiCallDataToKafka apiName svcProvider entityId req eithRes = do
  let res = either KUT.encodeToText KUT.encodeToText eithRes
  kafkaPushEnabled <- liftIO $ fromMaybe False . (readMaybe =<<) <$> lookupEnv "ENABLE_API_LOGS_KAFKA_PUSH"
  when kafkaPushEnabled $ pushExternalApiCallDataToKafkaWithTextEncodedResp apiName svcProvider entityId req res

pushExternalApiCallDataToKafkaWithTextEncodedResp :: forall m r req'. (MonadFlow m, MonadReader r m, ToJSON req', HasKafkaProducer r) => Text -> Text -> Maybe Text -> Maybe req' -> Text -> m ()
pushExternalApiCallDataToKafkaWithTextEncodedResp apiName svcProvider entityId req res = do
  KafkaProd.produceMessage ("External-API-Call-Logs", Nothing) =<< buildExternalAPICallLog
  where
    buildExternalAPICallLog = do
      id <- generateGUID
      timestamp <- getCurrentTime
      return $
        ExternalAPICallLog
          { requestPayload = KUT.encodeToText <$> req,
            responsePayload = res,
            createdAt = timestamp,
            updatedAt = timestamp,
            ..
          }

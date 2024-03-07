module Kernel.Beam.ART.ARTUtils where

import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import qualified Data.Text.Encoding as TE
import qualified Kafka.Producer as KafkaProd
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.IOLogging (LoggerEnv)

type HasARTFlow r = (HasField "loggerEnv" r LoggerEnv, HasField "shouldLogRequestId" r Bool, HasField "requestId" r (Maybe Text), HasField "kafkaProducerForART" r (Maybe KafkaProducerTools))

data RequestInfo' = RequestInfo'
  { requestMethod :: Text,
    rawPathInfo :: Text,
    rawQueryString :: Text,
    requestHeaders :: Text,
    body :: Text
  }
  deriving (Generic, Show, ToJSON)

data ArtData = ArtData
  { requestId :: Text,
    request :: Maybe RequestInfo',
    response :: Maybe Text,
    queryData :: Maybe QueryData,
    forkedTag :: Maybe Text,
    timestamp :: Maybe UTCTime
  }
  deriving (Show, Generic)

data QueryData = QueryData
  { queryType :: Text,
    setClause :: Text,
    whereClause :: Text,
    table :: Text,
    tableObject :: Text,
    kvEnabled :: Bool
  }
  deriving (Show, Generic, ToJSON)

instance ToJSON ArtData where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance Default ArtData where
  def = ArtData "" Nothing Nothing Nothing Nothing Nothing

pushToKafka :: Maybe KafkaProducerTools -> BL.ByteString -> Text -> Text -> IO ()
pushToKafka kafkaConn messageRecord topic key = do
  case kafkaConn of
    Nothing -> pure ()
    Just kafkaProducerTools' -> do
      void $ KafkaProd.produceMessage kafkaProducerTools'.producer (kafkaMessage topic messageRecord key)

kafkaMessage :: Text -> BL.ByteString -> Text -> KafkaProd.ProducerRecord
kafkaMessage topicName event key =
  KafkaProd.ProducerRecord
    { prTopic = KafkaProd.TopicName topicName,
      prPartition = KafkaProd.UnassignedPartition,
      prKey = Just $ TE.encodeUtf8 key,
      prValue = Just . BL.toStrict $ event
    }

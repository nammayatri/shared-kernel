module Kernel.Beam.ART.ARTUtils where

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import Data.Either (partitionEithers)
import qualified Data.Text.Encoding as TE
import qualified Kafka.Producer as KafkaProd
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.IOLogging (LoggerEnv)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

type HasARTFlow r = (HasField "loggerEnv" r LoggerEnv, HasField "shouldLogRequestId" r Bool, HasField "requestId" r (Maybe Text), HasField "kafkaProducerForART" r (Maybe KafkaProducerTools))

data RequestInfo' = RequestInfo'
  { requestMethod :: Text,
    rawPathInfo :: Text,
    rawQueryString :: Text,
    requestHeaders :: Text,
    body :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data ArtData = ArtData
  { requestId :: Text,
    request :: Maybe RequestInfo',
    response :: Maybe Text,
    queryData :: Maybe QueryData,
    forkedTag :: Maybe Text,
    timestamp :: Maybe UTCTime
  }
  deriving (Show, Generic, FromJSON)

data QueryData = QueryData
  { queryType :: Text,
    setClause :: Text,
    whereClause :: Text,
    table :: Text,
    tableObject :: Text,
    kvEnabled :: Bool,
    schemaName :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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

getCurrentFilePath :: FilePath -> IO FilePath
getCurrentFilePath fileName = do
  currentDir <- getCurrentDirectory
  return (currentDir </> fileName)

readAndDecodeArtData :: IO [ArtData]
readAndDecodeArtData = do
  let filePath = "/home/kv/projects/shared-kernel/lib/mobility-core/src/Kernel/Beam/data1.log"
  fileContent <- B.readFile filePath
  let jsonData = map (eitherDecode . BL.fromStrict) $ B.split '\n' fileContent :: [Either String ArtData]
  case partitionEithers jsonData of
    ([], decoded) -> pure decoded
    (err, _) -> error ("Failed to decode JSON data: " <> show err)

module Kernel.Beam.ART.ARTUtils where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Vector as V
import EulerHS.Language (MonadFlow)
import qualified EulerHS.Language as L
import qualified Kafka.Producer as KafkaProd
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.IOLogging (LoggerEnv)
import Kernel.Utils.Logging
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.FilePath (combine, splitFileName, (</>))
import System.IO (IOMode (AppendMode), withFile)

type HasARTFlow r = (HasField "loggerEnv" r LoggerEnv, HasField "shouldLogRequestId" r Bool, HasField "requestId" r (Maybe Text), HasField "kafkaProducerForART" r (Maybe KafkaProducerTools), HasField "isArtReplayerEnabled" r Bool)

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

data ArtProcessed = ArtProcessed
  { whereClauseText :: Text,
    dataTimestamp :: Maybe UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data QueryData = QueryData
  { queryType :: Text,
    setClause :: Text,
    whereClause :: [[(Text, Text)]],
    table :: Text,
    tableObject :: [Text],
    kvEnabled :: Bool,
    schemaName :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance ToJSON ArtData where
  toJSON = genericToJSON A.defaultOptions {A.omitNothingFields = True}

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

extractRequestId :: Maybe BL.ByteString -> Maybe Text
extractRequestId (Just jsonString)
  | Just (A.Object obj) <- A.decode jsonString,
    Just (A.Object intent) <- AKM.lookup "message" obj,
    Just (A.Object fulfillment) <- AKM.lookup "intent" intent,
    Just (A.Object tags) <- AKM.lookup "fulfillment" fulfillment,
    Just (A.Array tagsArray) <- AKM.lookup "tags" tags,
    Just (A.Object customRequestId) <- findCustomRequestIdObj tagsArray,
    Just (A.Array list) <- AKM.lookup "list" customRequestId,
    Just (A.Object listObj) <- list V.!? 0,
    Just (A.String value) <- AKM.lookup "value" listObj =
    Just value
  | otherwise = Nothing
extractRequestId Nothing = Nothing

findCustomRequestIdObj :: V.Vector A.Value -> Maybe A.Value
findCustomRequestIdObj tagsArray =
  V.find
    ( \tag -> case tag of
        A.Object tagObj -> case AKM.lookup "descriptor" tagObj of
          Just (A.Object descriptor) -> case AKM.lookup "code" descriptor of
            Just (A.String code) -> code == "CUSTOM_REQUEST_IDS"
            _ -> False
          _ -> False
        _ -> False
    )
    tagsArray

getCurrentFilePath :: FilePath -> IO FilePath
getCurrentFilePath fileName = do
  currentDir <- getCurrentDirectory
  return (currentDir </> fileName)

getFilePath :: FilePath -> IO FilePath
getFilePath = canonicalizePath

replaceLastFileName :: FilePath -> FilePath -> FilePath
replaceLastFileName path newFileName =
  let (dir, _) = splitFileName path
   in combine dir newFileName

readAndDecodeArtData :: (MonadFlow m) => m (Either String [ArtData])
readAndDecodeArtData = do
  filePath' <- L.getOption KBT.FilePathForART
  case filePath' of
    Nothing -> return $ Left "No file path for ART data found. Kindly provide the file path or set the environment variable ArtFilePath using setOption."
    Just filePath -> do
      fileContent <- L.runIO $ B.readFile filePath
      let jsonData = map (A.eitherDecode . BL.fromStrict) $ filter (not . B.null) $ B.split '\n' fileContent
      case partitionEithers jsonData of
        ([], decoded) -> return $ Right decoded
        (err, _) -> return $ Left $ "Failed to decode JSON data: " <> show err <> " in file: " <> show filePath

appendOrWriteToFile :: (MonadFlow m) => BL.ByteString -> m ()
appendOrWriteToFile messageRecord = do
  filePath' <- L.getOption KBT.FilePathForART
  case filePath' of
    Nothing -> do
      L.logError ("NO_FILE_PATH_FOR_ART" :: Text) $ "No file path for ART data found. Kindly provide the file path or set the environment variable ArtFilePath using setOption."
      pure ()
    Just filePath -> do
      let newFilePath = replaceLastFileName filePath "processedData.log"
      handle (\(e :: SomeException) -> L.logError ("FAILED_TO_WRITE_TO_FILE" :: Text) $ "Failed to write to file: " <> show e) $
        L.runIO $ withFile newFilePath AppendMode (\handle' -> BL.hPut handle' ("\n" <> messageRecord))

getProcessedData :: (MonadFlow m) => m (HM.HashMap Text (Maybe UTCTime))
getProcessedData = do
  filePath' <- L.getOption KBT.FilePathForART
  case filePath' of
    Nothing -> do
      L.logError ("NO_FILE_PATH_FOR_ART_PROCESSED" :: Text) $ "No file path for ART data found. Kindly provide the file path or set the environment variable ArtFilePath using setOption."
      pure HM.empty
    Just filePath -> do
      let newFilePath = replaceLastFileName filePath "processedData.log"
      fileContent <- L.runIO $ B.readFile newFilePath
      let jsonData = map (A.eitherDecode . BL.fromStrict) $ B.split '\n' fileContent :: [Either String ArtProcessed]
      case partitionEithers jsonData of
        ([], decoded) -> do
          let processedData = HM.fromList $ map (\(ArtProcessed whereClauseText' dataTimestamp') -> (whereClauseText', dataTimestamp')) decoded
          pure processedData
        (err, _) ->
          if B.null fileContent
            then pure HM.empty
            else do
              L.logError ("FAILED_TO_DECODE_PROCESSED_DATA" :: Text) $ "Failed to decode JSON data: " <> show err <> " in file: " <> show newFilePath
              pure HM.empty

checkProcessedQuery :: (MonadFlow m, Log m) => QueryData -> Maybe UTCTime -> Maybe Text -> Text -> m Bool
checkProcessedQuery queryData' timestamp schemaName' tableName' = do
  let whereClause' = show $ whereClause queryData'
      key = tableName' <> "-" <> fromMaybe "" schemaName' <> "-" <> whereClause'
  checkIfUsed <- getProcessedData
  case HM.lookup key checkIfUsed of
    Nothing -> pure False
    Just timestamp'' -> pure $ timestamp'' == timestamp

matchesWhereClause :: (MonadFlow m, Log m) => Text -> Text -> Maybe Text -> [[(Text, Text)]] -> (Maybe QueryData, Maybe UTCTime, Text) -> m Bool
matchesWhereClause queryType' tableName' schemaName' whereClause' (queryData, timestamp, requestId') =
  case queryData of
    Nothing -> pure False
    Just queryData' -> do
      if queryType' `T.isInfixOf` queryType queryData' && tableName' == table queryData' && schemaName' == schemaName queryData'
        then do
          -- lets get the HashMap of where clause and timestamp and check if that has been used
          checkIfUsed <- if (requestId' == "" || T.null requestId') then pure False else checkProcessedQuery queryData' timestamp schemaName' tableName'
          logDebug $ "Checking if the query has been used: " <> show checkIfUsed <> " for queryType: " <> queryType' <> " and whereClause: " <> show whereClause' <> " for table: " <> table queryData' <> " and schemaName: " <> fromMaybe "" (schemaName queryData')

          let flattenedWhereClause = DL.concat whereClause'
              flattenedWhereClauseArt = DL.concat $ whereClause queryData'
              lengthOfWhereClause = length flattenedWhereClause
              commonElements' = DL.intersect flattenedWhereClause flattenedWhereClauseArt
              commonElements = length commonElements'
              percentageMatch = if lengthOfWhereClause /= 0 then (fromIntegral commonElements / fromIntegral lengthOfWhereClause) * 100 :: Double else 100.0
          logDebug $ "Percentage match for ART: " <> show percentageMatch <> " for queryType: " <> queryType' <> " and whereClause: " <> show whereClause' <> " for table: " <> table queryData' <> " and schemaName: " <> fromMaybe "" (schemaName queryData')
          logDebug $ "Common elements for table " <> table queryData' <> " and schemaName: " <> fromMaybe "" (schemaName queryData') <> " are: " <> show commonElements'

          if percentageMatch > 90 && not checkIfUsed then pure $ True else pure $ False
        else pure False

getArtQueryObject :: (MonadFlow m, Log m) => Text -> Text -> Maybe Text -> [[(Text, Text)]] -> [(Maybe QueryData, Maybe UTCTime, Text)] -> m (Maybe QueryData)
getArtQueryObject queryType tableName' schemaName' whereClause' artDataList = do
  filteredData <- filterM (\artData -> matchesWhereClause queryType tableName' schemaName' whereClause' artData) artDataList
  case filteredData of
    [] -> pure Nothing
    ((queryData, timestamp, _) : _) -> do
      whenJust queryData $ \queryData' -> do
        let whereClause'' = show $ whereClause queryData'
            key = tableName' <> "-" <> fromMaybe "" schemaName' <> "-" <> whereClause''
        appendOrWriteToFile $ A.encode $ ArtProcessed key timestamp
      pure queryData

getTableIdentity :: Text -> Maybe Text -> [Maybe QueryData] -> Text -> Maybe [Text]
getTableIdentity tableName schemaName' queryDataList queryType' = do
  let data' = find (\queryData -> (tableName == table queryData) && (schemaName' == schemaName queryData) && queryType' `T.isInfixOf` queryType queryData) (catMaybes queryDataList)
  tableObject <$> data'

--------------------------------------------------------------------------------for Debugging--------------------------------------------------------------------------------

readAndDecodeArtData' :: IO (Either String [ArtData])
readAndDecodeArtData' = do
  let path = "path/to/file"
  fileContent <- B.readFile path
  let jsonData = map (A.eitherDecode . BL.fromStrict) $ B.split '\n' fileContent :: [Either String ArtData]
  case partitionEithers jsonData of
    ([], decoded) -> return $ Right decoded
    (err, _) -> return $ Left ("Failed to decode JSON data: " <> show err <> " in file: " <> show path)

printART :: IO ()
printART = do
  parsedData <- readAndDecodeArtData'
  artData <- either (const $ return []) return parsedData
  print artData

{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Kernel.Beam.ART.ARTUtils where

import Control.Monad.Extra (findM)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default.Class
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Sequence as Seq
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import qualified Kafka.Producer as KafkaProd
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.IOLogging (LoggerEnv)
import Kernel.Utils.Logging
import Kernel.Utils.Text
import qualified Network.HTTP.Types as HTTP
import Servant.Client.Core
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

data BlackListedColumns = BlackListedColumns
  { tableName :: Text,
    schemaName :: Maybe Text,
    columns :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BlackListedColumnList = BlackListedColumnList
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity BlackListedColumnList [BlackListedColumns]

data ForkedTag = ForkedTag
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity ForkedTag Text

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
findCustomRequestIdObj = V.find $ \case
  A.Object tagObj ->
    case AKM.lookup "descriptor" tagObj of
      Just (A.Object descriptor) ->
        case AKM.lookup "code" descriptor of
          Just (A.String code) -> code == "CUSTOM_REQUEST_IDS"
          _ -> False
      _ -> False
  _ -> False

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

readAndDecodeArtData :: (L.MonadFlow m, Log m) => m (Either String [ArtData])
readAndDecodeArtData = do
  filePath' <- L.getOption KBT.FilePathForART
  forkedTag <- L.getOptionLocal ForkedTag
  case filePath' of
    Nothing -> return $ Left "No file path for ART data found. Kindly provide the file path or set the environment variable ArtFilePath using setOption."
    Just filePath -> do
      let newFilePath = maybe filePath (\tag -> replaceLastFileName filePath $ T.unpack ("ArtForked/" <> tag <> ".log")) forkedTag
      logDebug $ "Reading ART data from file: " <> show newFilePath
      fileContent <- L.runIO $ B.readFile newFilePath
      let jsonData = map (A.eitherDecode . BL.fromStrict) $ filter (not . B.null) $ B.split '\n' fileContent
      case partitionEithers jsonData of
        ([], decoded) -> return $ Right decoded
        (err, _) -> return $ Left $ "Failed to decode JSON data: " <> show err <> " in file: " <> show newFilePath

appendOrWriteToFile :: (L.MonadFlow m) => BL.ByteString -> m ()
appendOrWriteToFile messageRecord = do
  filePath' <- L.getOption KBT.FilePathForART
  case filePath' of
    Nothing -> do
      L.logError ("NO_FILE_PATH_FOR_ART" :: Text) $ "No file path for ART data found. Kindly provide the file path or set the environment variable ArtFilePath using setOption."
      pure ()
    Just filePath -> do
      let newFilePath = replaceLastFileName filePath "processedData.log"
      writeToFileSafely newFilePath messageRecord

getRedisWhereClauseValue :: [[(Text, Text)]] -> Text
getRedisWhereClauseValue [whereClause] = do
  case listToMaybe whereClause of
    Just (_, value) -> value
    _ -> ""
getRedisWhereClauseValue _ = ""

writeToFileSafely :: (L.MonadFlow m) => FilePath -> BL.ByteString -> m ()
writeToFileSafely filePath messageRecord = do
  fileContent <- try $ L.runIO $ withFile filePath AppendMode (\handle' -> BL.hPut handle' ("\n" <> messageRecord))
  case fileContent of
    Left (e :: SomeException) -> do
      L.logError ("FAILED_TO_WRITE_TO_FILE" :: Text) $ "Failed to write to file ... Trying again with error: " <> show e
      L.runIO $ threadDelay 1000000
      writeToFileSafely filePath messageRecord
    Right _ -> pure ()

getProcessedData :: (L.MonadFlow m) => m (HM.HashMap Text (Maybe UTCTime))
getProcessedData = do
  filePath' <- L.getOption KBT.FilePathForART
  case filePath' of
    Nothing -> do
      L.logError ("NO_FILE_PATH_FOR_ART_PROCESSED" :: Text) $ "No file path for ART data found. Kindly provide the file path or set the environment variable ArtFilePath using setOption."
      pure HM.empty
    Just filePath -> do
      let newFilePath = replaceLastFileName filePath "processedData.log"
      fileContent <- readFileSafely newFilePath
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

readFileSafely :: (L.MonadFlow m) => FilePath -> m B.ByteString
readFileSafely filePath = do
  fileContent <- try $ L.runIO $ B.readFile filePath
  case fileContent of
    Left (e :: SomeException) -> do
      L.logError ("FAILED_TO_READ_FILE" :: Text) $ "Failed to read file ... Trying again with error: " <> show e
      L.runIO $ threadDelay 1000000
      readFileSafely filePath
    Right content -> pure content

checkProcessedQuery :: (L.MonadFlow m, Log m) => QueryData -> Maybe UTCTime -> Maybe Text -> Text -> Maybe Text -> m Bool
checkProcessedQuery queryData' timestamp schemaName' tableName' forkedTag = do
  let whereClause' = show $ whereClause queryData'
      key = tableName' <> "-" <> fromMaybe "" schemaName' <> "-" <> whereClause' <> "-" <> show timestamp <> "-" <> fromMaybe "" forkedTag
  checkIfUsed <- getProcessedData
  let look = HM.lookup key checkIfUsed
  pure $ isJust look

checkWhereClauseMatching :: (L.MonadFlow m, Log m) => [[(Text, Text)]] -> [[(Text, Text)]] -> m Bool
checkWhereClauseMatching whereClause' whereClauseArt' = do
  let flattenedWhereClause = DL.concat whereClause'
      flattenedWhereClauseArt = DL.concat whereClauseArt'
      flattenedColumnList = map fst flattenedWhereClause
      flattenedColumnListArt = map fst flattenedWhereClauseArt
      commonElements' = flattenedColumnList `DL.intersect` flattenedColumnListArt
      lengthOfWhereClause = length flattenedColumnList
      lengthOfCommonElements = length commonElements'
  pure $ lengthOfCommonElements == lengthOfWhereClause

matchesWhereClause :: (L.MonadFlow m, Log m) => Text -> Text -> Maybe Text -> [[(Text, Text)]] -> (Maybe QueryData, Maybe UTCTime, Text, Maybe Text) -> m Bool
matchesWhereClause queryType' tableName' schemaName' whereClause' (queryData, timestamp, requestId', tag) =
  case queryData of
    Nothing -> pure False
    Just queryData' -> do
      forkedTag' <- L.getOptionLocal ForkedTag
      if queryType' `T.isInfixOf` queryType queryData' && tableName' == table queryData' && schemaName' == queryData'.schemaName && tag == forkedTag'
        then do
          -- lets get the HashMap of where clause and timestamp and check if that has been used
          checkIfUsed <- if requestId' == "" || T.null requestId' then pure False else checkProcessedQuery queryData' timestamp schemaName' tableName' forkedTag'
          checkWhereClause <- checkWhereClauseMatching whereClause' (whereClause queryData')
          let matchWhereClauseCondition = checkWhereClause && not checkIfUsed
          logDebug $ "Matched with Where Clause : " <> show matchWhereClauseCondition <> " for table: " <> table queryData' <> " with whereClauseArt is " <> show (whereClause queryData') <> " and WhereClause is " <> show whereClause' <> " and checkWhereClause is " <> show checkWhereClause <> " and checkIfUsed is " <> show checkIfUsed <> " and tag is " <> show tag <> " and forkedTag is " <> show forkedTag'
          pure matchWhereClauseCondition
        else pure False

getArtQueryObject :: (L.MonadFlow m, Log m) => Text -> Text -> Maybe Text -> [[(Text, Text)]] -> [(Maybe QueryData, Maybe UTCTime, Text, Maybe Text)] -> m (Maybe QueryData)
getArtQueryObject queryType tableName' schemaName' whereClause' artDataList = do
  filteredData <- findM (matchesWhereClause queryType tableName' schemaName' whereClause') artDataList
  case filteredData of
    Just (queryData, timestamp, _, forkedTag) -> do
      whenJust queryData $ \queryData' -> do
        let whereClause'' = show $ whereClause queryData'
            key = tableName' <> "-" <> fromMaybe "" schemaName' <> "-" <> whereClause'' <> "-" <> show timestamp <> "-" <> fromMaybe "" forkedTag
        appendOrWriteToFile $ A.encode $ ArtProcessed key timestamp
      pure queryData
    Nothing -> do
      logDebug $ "No match found for table: " <> tableName' <> " with whereClauseArt is " <> show whereClause' <> "whereClause is " <> show whereClause' <> " and schemaName is " <> show schemaName' <> " and queryType is " <> queryType
      pure Nothing

getTableIdentity :: Text -> Maybe Text -> [Maybe QueryData] -> Text -> Maybe [Text]
getTableIdentity tableName schemaName' queryDataList queryType' = do
  let data' = find (\queryData -> (tableName == table queryData) && (schemaName' == queryData.schemaName) && queryType' `T.isInfixOf` queryType queryData) (catMaybes queryDataList)
  tableObject <$> data'

---------------------------------------------------------------------redis functions---------------------------------------------------------------------

concatWithDelimiter :: Text -> [(BS.ByteString, BS.ByteString)] -> [BS.ByteString]
concatWithDelimiter delimiter = map (BS.intercalate (TE.encodeUtf8 delimiter) . toList)

splitWithDelimiter :: Text -> [Text] -> [[Text]]
splitWithDelimiter delimiter = map (T.splitOn delimiter)

buildKeyArt :: HedisFlow m env => Text -> m Text
buildKeyArt key = do
  keyModifier <- asks (.hedisEnv.keyModifier)
  return . cs $ keyModifier key

getArt :: (FromJSON a, HedisFlow m env) => Text -> Text -> m (Maybe a)
getArt callType keyRedis = do
  redisObject' <- getRedisObject callType keyRedis
  case listToMaybe redisObject' of
    Nothing -> pure Nothing
    Just redisObject'' -> do
      case A.decode $ BL.fromStrict $ TE.encodeUtf8 redisObject'' of
        Just res -> pure $ Just res
        _ -> do
          L.logError ("ART_REDIS_KEY_DATA_DECODE_ERROR" :: Text) $ "Art data decode error for key: " <> keyRedis <> " with callType: " <> callType <> " with data: " <> cs redisObject''
          pure Nothing

getListArt :: (FromJSON a, HedisFlow m env) => Text -> Text -> m [a]
getListArt callType keyRedis = do
  redisObject' <- getRedisObject callType keyRedis
  case redisObject' of
    [] -> pure []
    _ -> do
      let res = mapMaybe (A.decode . BL.fromStrict . TE.encodeUtf8) redisObject'
      pure res

hGetAllArt :: (FromJSON a, HedisFlow m env) => Text -> Text -> m [(Text, a)]
hGetAllArt callType keyRedis = do
  redisObject' <- getRedisObject callType keyRedis
  let keyValue = splitWithDelimiter (T.pack "~~") redisObject'
  let res = mapMaybe getDataForHgetAll keyValue
  case res of
    [] -> do
      L.logDebug ("ART_REDIS_KEY_DATA_NOT_FOUND_ALL" :: Text) $ "Art data not found for key: " <> keyRedis <> " with callType: " <> callType
      pure []
    _ -> pure res
  where
    getDataForHgetAll res = case res of
      key : value : _ -> case A.decode $ BL.fromStrict $ TE.encodeUtf8 value of
        Just ress -> Just (key, ress)
        _ -> Nothing
      _ -> Nothing

getRedisObject :: (HedisFlow m env) => Text -> Text -> m [Text]
getRedisObject callType keyRedis = do
  artData <- readAndDecodeArtData
  let errorTag = "ART_REDIS_ERROR"
      key = [[("key", keyRedis)]]
  case artData of
    Left err -> do
      L.logError ("ART_DATA_PARSE_ERROR_OR_NOT_FOUND_" <> errorTag) $ "Art data not found for key: " <> keyRedis <> " schema: " <> " where: " <> show key <> " with error: " <> show err
      throwError $ InternalError (("ART_DATA_PARSE_ERROR_OR_NOT_FOUND_" <> errorTag) <> show err)
    Right artData' -> do
      let artDataList = map (\artdata -> (queryData artdata, timestamp artdata, requestId artdata, forkedTag artdata)) artData'
      queryData' <- getArtQueryObject callType "redis" Nothing key artDataList
      logRedisQueryData callType keyRedis (maybe [] (map TE.encodeUtf8 . tableObject) queryData')
      case queryData' of
        Nothing -> do
          L.logWarning ("ART_REDIS_DATA_NOT_FOUND_" <> errorTag) $ "Art data not found for key: " <> keyRedis <> " schema: " <> " where: " <> show key
          pure []
        Just queryData'' -> pure $ tableObject queryData''

logRedisQueryData :: (HedisFlow m env) => Text -> Text -> [BS.ByteString] -> m ()
logRedisQueryData queryType keyRedis value = do
  shouldLogRequestId <- asks (.shouldLogRequestId)
  timestamp <- getCurrentTime
  forkedTag <- L.getOptionLocal ForkedTag
  when shouldLogRequestId $ do
    fork "ArtData" $ do
      kafkaConn <- L.getOption KBT.KafkaConn
      requestId <- fromMaybe "" <$> asks (.requestId)
      let whereClause = [[("key", keyRedis)]]
          setClause = ""
          tableObject = map TE.decodeUtf8 value
          queryData = QueryData queryType setClause whereClause "redis" tableObject False Nothing
      handle (\(e :: SomeException) -> L.logError ("ART_QUERY_LOG_FAILED" :: Text) $ "Error while logging redis query data: " <> show e) $ do
        liftIO $ pushToKafka kafkaConn (A.encode def {requestId = requestId, queryData = Just queryData, timestamp = Just timestamp, forkedTag = forkedTag}) "ART-Logs" requestId

--------------------------------------------------------------------------------API functions--------------------------------------------------------------------------------

logApiResponseData :: (Forkable m, L.MonadFlow m, ToJSON a, HasCoreMetrics r, MonadReader r m) => Text -> a -> m ()
logApiResponseData url response = do
  shouldLogRequestId <- asks (.shouldLogRequestId)
  forkedTag <- L.getOptionLocal ForkedTag
  when shouldLogRequestId $ do
    fork "ArtData" $ do
      let whereClause = [[(url, "url")]]
          setClause = ""
          tableObject = [encodeToText response]
      kafkaConn <- L.getOption KBT.KafkaConn
      requestId <- fromMaybe "" <$> asks (.requestId)
      timestamp <- L.runIO getCurrentTime
      let queryData = QueryData "response" setClause whereClause "api" tableObject False Nothing
      handle (\(e :: SomeException) -> L.logError ("ART_QUERY_LOG_FAILED" :: Text) $ "Error while logging api response data: " <> show e) $ do
        L.runIO $ pushToKafka kafkaConn (A.encode def {requestId = requestId, queryData = Just queryData, timestamp = Just timestamp, forkedTag = forkedTag}) "ART-Logs" requestId

getIsArtReplayerEnabled :: (L.MonadFlow m, HasCoreMetrics r, MonadReader r m) => m Bool
getIsArtReplayerEnabled = do
  isArtReplayerEnabled <- asks (.isArtReplayerEnabled)
  pure isArtReplayerEnabled

exampleResponse :: Response
exampleResponse =
  Response
    { responseStatusCode = HTTP.status200,
      responseHeaders = Seq.empty,
      responseHttpVersion = HTTP.HttpVersion 1 1,
      responseBody = LBS.pack "ERROR: No response found for ART replay"
    }

getArtReplayResponse :: (Forkable m, L.MonadFlow m, FromJSON res, HasCoreMetrics r, MonadReader r m, Log m) => Text -> m (Either ClientError res)
getArtReplayResponse url = do
  let whereClause = [[(url, "url")]]
  artReplayResponse <- readAndDecodeArtData
  case artReplayResponse of
    Right response -> do
      let artDataList = map (\artdata -> (queryData artdata, timestamp artdata, requestId artdata, forkedTag artdata)) response
      queryData <- getArtQueryObject "response" "api" Nothing whereClause artDataList
      case queryData of
        Nothing -> return $ Left $ DecodeFailure ("No response found for ART replay" :: Text) exampleResponse
        Just queryData' -> do
          let apiObject = queryData'.tableObject
              apiResponse = map decodeFromText apiObject
              apiResponse' = catMaybes apiResponse
          case listToMaybe apiResponse' of
            Just response' -> return $ Right response'
            Nothing -> return $ Left $ DecodeFailure ("No response found for ART replay" :: Text) exampleResponse
    Left err -> do
      logError $ "Error occured during ART replay: " <> show err
      return $ Left $ DecodeFailure ("Error occured during ART replay" :: Text) exampleResponse

--------------------------------------------------------------------------------for Debugging--------------------------------------------------------------------------------

readAndDecodeArtData' :: IO (Either String [ArtData])
readAndDecodeArtData' = do
  let path = "path/to/your/file.log"
  fileContent <- B.readFile path
  let jsonData = map (A.eitherDecode . BL.fromStrict) $ B.split '\n' fileContent :: [Either String ArtData]
  case partitionEithers jsonData of
    ([], decoded) -> return $ Right decoded
    (err, _) -> return $ Left ("Failed to decode JSON data: " <> show err <> " in file: " <> show path <> "at line" <> show (length jsonData))

printART :: IO ()
printART = do
  parsedData <- readAndDecodeArtData'
  print parsedData

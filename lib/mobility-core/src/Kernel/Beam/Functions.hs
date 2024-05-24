module Kernel.Beam.Functions
  ( FromTType' (..),
    ToTType' (..),
    FromTType'' (..),
    ToTType'' (..),
    FromCacType (..),
    meshConfig,
    runInReplica,
    getMasterBeamConfig,
    getLocationDbBeamConfig,
    findOneWithKV,
    findOneWithKVScheduler,
    findAllWithKV,
    findAllWithKVScheduler,
    findAllWithOptionsKV,
    findAllWithOptionsKV',
    findAllWithOptionsKVScheduler,
    findOneWithDb, -- not used
    findAllWithDb,
    findAllWithOptionsDb,
    updateWithKV,
    updateWithKVScheduler,
    updateOneWithKV,
    createWithKV,
    createWithKVScheduler,
    deleteWithKV,
    deleteWithDb, -- not used
    findAllWithKVAndConditionalDB,
    getDbFunctions,
    getArtDbFunctions,
  )
where

import Data.Aeson
import Data.Default.Class
import Database.Beam hiding (primaryKey, timestamp)
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (DBCommandVersion' (..), MeshConfig (..), MeshMeta (..))
import EulerHS.KVConnector.Utils
import qualified EulerHS.Language as L
import EulerHS.Types hiding (Log)
import qualified Kernel.Beam.ART.ARTUtils as ART
import Kernel.Beam.Lib.Utils
import Kernel.Beam.Types
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Text (decodeFromTextArt, encodeToTextArt)
import Sequelize
import System.Random

----------------------- critical functions -----------------------

runInReplica :: (L.MonadFlow m, Log m) => m a -> m a
runInReplica m = do
  L.setOptionLocal ReplicaEnabled True
  res <- m
  L.setOptionLocal ReplicaEnabled False
  pure res

getDbFunctions :: DbFunctions
getDbFunctions =
  DbFunctions
    { createInternalFunction = createInternal,
      findOneInternalFunction = findOneInternal,
      findAllInternalFunction = findAllInternal,
      findAllWithOptionsInternalFunction = findAllWithOptionsInternal,
      findAllWithOptionsInternalFunction' = findAllWithOptionsInternal',
      findAllWithKVAndConditionalDBInternalFunction = findAllWithKVAndConditionalDBInternal,
      updateInternalFunction = updateInternal,
      updateOneInternalFunction = updateOneInternal,
      deleteInternalFunction = deleteInternal
    }

getArtDbFunctions :: DbFunctions
getArtDbFunctions =
  DbFunctions
    { createInternalFunction = createInternalArt,
      findOneInternalFunction = findOneInternalArt,
      findAllInternalFunction = findAllInternalArt,
      findAllWithOptionsInternalFunction = findAllWithOptionsInternalArt,
      findAllWithOptionsInternalFunction' = findAllWithOptionsInternalArt',
      findAllWithKVAndConditionalDBInternalFunction = findAllWithKVAndConditionalDBInternalArt,
      updateInternalFunction = updateInternalArt,
      updateOneInternalFunction = updateOneInternalArt,
      deleteInternalFunction = deleteInternalArt
    }

setMeshConfig :: (L.MonadFlow m, HasCallStack) => Text -> Maybe Text -> MeshConfig -> m MeshConfig
setMeshConfig modelName mSchema meshConfig' = do
  schema <- maybe (L.throwException $ InternalError "Schema not found") pure mSchema
  let redisStream = if schema == "atlas_driver_offer_bpp" then "driver-db-sync-stream" else "rider-db-sync-stream" -- lets change when we enable for dashboards
  tables <- L.getOption KBT.Tables
  randomIntV <- L.runIO (randomRIO (1, 100) :: IO Int)
  case tables of
    Nothing -> L.throwException $ InternalError "Tables not found"
    Just tables' -> do
      let enableKVForWriteAlso = tables'.enableKVForWriteAlso
          enableKVForRead = tables'.enableKVForRead
          tableObject = find (\table' -> nameOfTable table' == modelName) enableKVForWriteAlso
      updatedMeshConfig <- case tableObject of
        Nothing -> pure $ meshConfig' {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead, ecRedisDBStream = redisStream}
        Just table' -> do
          let redisTtl' = fromMaybe (meshConfig'.redisTtl) (table'.redisTtl)
          if fromIntegral (percentEnable table') >= randomIntV
            then pure $ meshConfig' {meshEnabled = True, kvHardKilled = modelName `notElem` enableKVForRead, ecRedisDBStream = redisStream, redisTtl = redisTtl'}
            else pure $ meshConfig' {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead, ecRedisDBStream = redisStream}
      L.logDebug ("setMeshConfig" :: Text) $ "meshConfig for table: " <> modelName <> " : " <> show updatedMeshConfig
      pure updatedMeshConfig

withUpdatedMeshConfig :: forall table m a. (L.MonadFlow m, HasCallStack, ModelMeta table) => Proxy table -> (MeshConfig -> m a) -> m a
withUpdatedMeshConfig _ mkAction = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  mkAction updatedMeshConfig

getMasterDBConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getMasterDBConfig = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCnf' -> pure dbCnf'
    Nothing -> L.throwException $ InternalError "masterDb Config not found"

getLocDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getLocDbConfig = do
  dbConf <- L.getOption KBT.PsqlLocDbCfg
  case dbConf of
    Just dbCnf' -> pure dbCnf'
    Nothing -> L.throwException $ InternalError "LocationDb Config not found"

getMasterBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
getMasterBeamConfig = do
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  conn <- L.getOrInitSqlConn dbConf
  case conn of
    Right conn' -> pure conn'
    Left _ -> L.throwException $ InternalError "MasterDb Beam Config not found"

getLocationDbBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
getLocationDbBeamConfig = do
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf <- maybe getLocDbConfig (\inReplica' -> if inReplica' then getReplicaLocationDbConfig else getLocDbConfig) inReplica
  conn <- L.getOrInitSqlConn dbConf
  case conn of
    Right conn' -> pure conn'
    Left _ -> L.throwException $ InternalError "LocationDb Beam Config not found"

----- replica db functions---------------

getReplicaDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getReplicaDbConfig = do
  dbConf <- L.getOption KBT.PsqlDbCfgR1
  case dbConf of
    Just dbCnf' -> pure dbCnf'
    Nothing -> L.throwException $ InternalError "ReplicaDb Config not found"

getReplicaLocationDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getReplicaLocationDbConfig = do
  dbConf <- L.getOption KBT.PsqlLocReplicaDbCfg
  case dbConf of
    Just dbCnf' -> pure dbCnf'
    Nothing -> L.throwException $ InternalError "Replica LocationDB Config not found"

-- findOne --

findOneWithKV ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKV where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findOneInternalFunction updatedMeshConfig fromTType' where'

findOneWithKVScheduler ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType'' (table Identity) a
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKVScheduler where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findOneInternalFunction updatedMeshConfig fromTType'' where'

findOneWithDb ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithDb where' = do
  DbFunctions {..} <- asks (.dbFunctions)
  findOneInternalFunction meshConfig fromTType' where'

-- findAll --

findAllWithKV ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m [a]
findAllWithKV where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllInternalFunction updatedMeshConfig fromTType' where'

findAllWithKVScheduler ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType'' (table Identity) a
  ) =>
  Where Postgres table ->
  m [a]
findAllWithKVScheduler where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllInternalFunction updatedMeshConfig fromTType'' where'

findAllWithDb ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m [a]
findAllWithDb where' = do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllInternalFunction meshConfig fromTType' where'

findAllWithKVAndConditionalDB ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  Maybe (OrderBy table) ->
  m [a]
findAllWithKVAndConditionalDB where' orderBy = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllWithKVAndConditionalDBInternalFunction updatedMeshConfig fromTType' where' orderBy

-- findAllWithOptions --

findAllWithOptionsKV ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKV where' orderBy mbLimit mbOffset = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllWithOptionsInternalFunction updatedMeshConfig fromTType' where' orderBy mbLimit mbOffset

findAllWithOptionsKV' ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKV' where' mbLimit mbOffset = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllWithOptionsInternalFunction' updatedMeshConfig fromTType' where' mbLimit mbOffset

findAllWithOptionsKVScheduler ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType'' (table Identity) a
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKVScheduler where' orderBy mbLimit mbOffset = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllWithOptionsInternalFunction updatedMeshConfig fromTType'' where' orderBy mbLimit mbOffset

findAllWithOptionsDb ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsDb where' orderBy mbLimit mbOffset = do
  DbFunctions {..} <- asks (.dbFunctions)
  findAllWithOptionsInternalFunction meshConfig fromTType' where' orderBy mbLimit mbOffset

-- update --

updateWithKV ::
  forall table m r.
  (BeamTableFlow table m r, KvDbFlow m r) =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKV setClause whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  updateInternalFunction updatedMeshConfig setClause whereClause

updateWithKVScheduler ::
  forall table m r.
  (BeamTableFlow table m r, KvDbFlow m r) =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKVScheduler setClause whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  updateInternalFunction updatedMeshConfig setClause whereClause

-- updateOne --

updateOneWithKV ::
  forall table m r.
  (BeamTableFlow table m r, KvDbFlow m r) =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneWithKV setClause whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  updateOneInternalFunction updatedMeshConfig setClause whereClause

-- create --

createWithKV ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    ToTType' (table Identity) a
  ) =>
  a ->
  m ()
createWithKV a = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  createInternalFunction updatedMeshConfig toTType' a

createWithKVScheduler ::
  forall table m r a.
  ( BeamTableFlow table m r,
    KvDbFlow m r,
    ToTType'' (table Identity) a
  ) =>
  a ->
  m ()
createWithKVScheduler a = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  createInternalFunction updatedMeshConfig toTType'' a

-- delete --

deleteWithKV ::
  forall table m r.
  (BeamTableFlow table m r, KvDbFlow m r) =>
  Where Postgres table ->
  m ()
deleteWithKV whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  DbFunctions {..} <- asks (.dbFunctions)
  deleteInternalFunction updatedMeshConfig whereClause

deleteWithDb ::
  forall table m r.
  (BeamTableFlow table m r, KvDbFlow m r) =>
  Where Postgres table ->
  m ()
deleteWithDb where' = do
  DbFunctions {..} <- asks (.dbFunctions)
  deleteInternalFunction meshConfig where'

-- internal --

findOneInternal ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m (Maybe a)
findOneInternal updatedMeshConfig fromTType where' = do
  now <- getCurrentTime
  dbConf' <- getReadDBConfigInternal
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> do
      logQueryData "findOneInternal" where' [] [res] (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
      fromTType res
    Right Nothing -> logQueryData "findOneInternal" where' [] [] (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now >> pure Nothing
    Left err -> throwError $ InternalError $ show err

findAllInternal ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m [a]
findAllInternal updatedMeshConfig fromTType where' = do
  now <- getCurrentTime
  dbConf' <- getReadDBConfigInternal
  result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- do
        logQueryData "findAllInternal" where' [] res (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
        mapM fromTType res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsInternal ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsInternal updatedMeshConfig fromTType where' orderBy mbLimit mbOffset = do
  now <- getCurrentTime
  dbConf' <- getReadDBConfigInternal
  result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- do
        logQueryData "findAllWithOptionsInternal" where' [] res (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
        mapM fromTType res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsInternal' ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsInternal' updatedMeshConfig fromTType where' mbLimit mbOffset = do
  now <- getCurrentTime
  dbConf' <- getReadDBConfigInternal
  result <- KV.findAllWithOptionsKVConnector' dbConf' updatedMeshConfig where' mbLimit mbOffset
  case result of
    Right res -> do
      res' <- do
        logQueryData "findAllWithOptionsInternal'" where' [] res (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
        mapM fromTType res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithKVAndConditionalDBInternal ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  Maybe (OrderBy table) ->
  m [a]
findAllWithKVAndConditionalDBInternal updatedMeshConfig fromTType where' orderBy = do
  now <- getCurrentTime
  dbConf' <- getReadDBConfigInternal
  result <- KV.findAllWithKVAndConditionalDBInternal dbConf' updatedMeshConfig where' orderBy
  case result of
    Right res -> do
      res' <- do
        logQueryData "findAllWithKVAndConditionalDBInternal" where' [] res (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
        mapM fromTType res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

getReadDBConfigInternal :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getReadDBConfigInternal = do
  inReplica <- L.getOptionLocal ReplicaEnabled
  maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica

updateInternal ::
  forall table m r.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateInternal updatedMeshConfig setClause whereClause = do
  now <- getCurrentTime
  dbConf <- getMasterDBConfig
  res <- KV.updateAllReturningWithKVConnector dbConf updatedMeshConfig setClause whereClause
  logQueryData "updateInternal" whereClause setClause [] (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
  case res of
    Right res' -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Updated rows KV: " <> show res'
        else do
          topicName <- getKafkaTopic (modelSchemaName @table) (modelTableName @table)
          let mappings = getMappings res'
          handle (\(e :: SomeException) -> L.logError ("KAFKA_PUSH_FAILED" :: Text) $ "Kafka push error while update:  " <> show e <> "in topic" <> topicName) $
            mapM_ (\object' -> void $ pushToKafka (replaceMappings (toJSON object') mappings) topicName (getKeyForKafka $ getLookupKeyByPKey object')) res'
          logDebug $
            "Updated rows DB: " <> show res'
    Left err -> throwError $ InternalError $ show err

updateOneInternal ::
  forall table m r.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneInternal updatedMeshConfig setClause whereClause = do
  now <- getCurrentTime
  dbConf <- getMasterDBConfig
  res <- KV.updateWithKVConnector dbConf updatedMeshConfig setClause whereClause
  logQueryData "updateOneInternal" whereClause setClause [] (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
  case res of
    Right obj -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Updated row KV: " <> show obj
        else do
          whenJust obj $ \object' -> do
            topicName <- getKafkaTopic (modelSchemaName @table) (modelTableName @table)
            let newObject = replaceMappings (toJSON object') (getMappings [object'])
            handle (\(e :: SomeException) -> L.logError ("KAFKA_PUSH_FAILED" :: Text) $ "Kafka push error while update: " <> show e <> "in topic" <> topicName) $
              void $ pushToKafka newObject topicName (getKeyForKafka $ getLookupKeyByPKey object')
            logDebug $
              "Updated row DB: " <> show obj
    Left err -> throwError $ InternalError $ show err

createInternal ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (a -> table Identity) ->
  a ->
  m ()
createInternal updatedMeshConfig toTType a = do
  now <- getCurrentTime
  let tType = toTType a
  dbConf' <- getMasterDBConfig
  result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
  logQueryData "createInternal" [] [] [tType] (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
  case result of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Created row in KV: " <> show tType
        else do
          topicName <- getKafkaTopic (modelSchemaName @table) (modelTableName @table)
          let newObject = replaceMappings (toJSON tType) (getMappings [tType])
          handle (\(e :: SomeException) -> L.logError ("KAFKA_PUSH_FAILED" :: Text) $ "Kafka push error while create: " <> show e <> "in topic" <> topicName) $
            void $ pushToKafka newObject topicName (getKeyForKafka $ getLookupKeyByPKey tType)
          logDebug $
            "Created row in DB: " <> show tType
    Left err -> throwError $ InternalError $ show err

deleteInternal ::
  forall table m r.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  Where Postgres table ->
  m ()
deleteInternal updatedMeshConfig whereClause = do
  now <- getCurrentTime
  dbConf <- getMasterDBConfig
  res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
  logQueryData "deleteInternal" whereClause [] [] (meshEnabled updatedMeshConfig) (modelTableName @table) (modelSchemaName @table) now
  case res of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Deleted rows in KV: " <> show res
        else logDebug $ "Deleted rows in DB: " <> show res
    Left err -> throwError $ InternalError $ show err

logQueryData ::
  ( EsqDBFlow m r,
    ToJSON (table Identity),
    Model Postgres table,
    MeshMeta Postgres table
  ) =>
  Text ->
  Where Postgres table ->
  [Set Postgres table] ->
  [table Identity] ->
  Bool ->
  Text ->
  Maybe Text ->
  UTCTime ->
  m ()
logQueryData queryType whereClause' setClause' tableObject' kvEnabled table schemaName timestamp = do
  shouldLogRequestId <- asks (.shouldLogRequestId)
  forkedTag <- L.getOptionLocal ART.ForkedTag
  when shouldLogRequestId $
    fork "ArtData" $ do
      kafkaConn <- L.getOption KBT.KafkaConn
      requestId <- fromMaybe "" <$> asks (.requestId)
      let whereClause = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause')
          setClause = show $ jsonKeyValueUpdates V1' setClause'
          tableObject = map encodeToTextArt tableObject'
          queryData = ART.QueryData {..}
      handle (\(e :: SomeException) -> L.logError ("ART_QUERY_LOG_FAILED" :: Text) $ "Error while logging query data: " <> show e) $ do
        liftIO $ ART.pushToKafka kafkaConn (encode def {ART.requestId = requestId, ART.queryData = Just queryData, ART.timestamp = Just timestamp, ART.forkedTag = forkedTag}) "ART-Logs" requestId

{--
------------------------------------------------------------------------------------------------------------------
             _____    _______       ______   _    _   _   _    _____   _______   _____    ____    _   _    _____
     /\     |  __ \  |__   __|     |  ____| | |  | | | \ | |  / ____| |__   __| |_   _|  / __ \  | \ | |  / ____|
    /  \    | |__) |    | |        | |__    | |  | | |  \| | | |         | |      | |   | |  | | |  \| | | (___
   / /\ \   |  _  /     | |        |  __|   | |  | | | . ` | | |         | |      | |   | |  | | | . ` |  \___ \
  / ____ \  | | \ \     | |        | |      | |__| | | |\  | | |____     | |     _| |_  | |__| | | |\  |  ____) |
 /_/    \_\ |_|  \_\    |_|        |_|       \____/  |_| \_|  \_____|    |_|    |_____|  \____/  |_| \_| |_____/

------------------------------------------------------------------------------------------------------------------
--}

findOneInternalArt ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m (Maybe a)
findOneInternalArt _ ttype where' = listToMaybe <$> getTableObject @table "FIND_ONE" "findOneInternal" ttype where'

findAllInternalArt ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m [a]
findAllInternalArt _ = getTableObject @table "FIND_ALL" "findAllInternal"

findAllWithOptionsInternalArt ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsInternalArt _ ttype where' _ _ _ = getTableObject @table "FIND_ALL_OPTION" "findAllWithOptionsInternal" ttype where'

findAllWithOptionsInternalArt' ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsInternalArt' _ ttype where' _ _ = getTableObject @table "FIND_ALL_OPTION_'" "findAllWithOptionsInternal'" ttype where'

findAllWithKVAndConditionalDBInternalArt ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  Maybe (OrderBy table) ->
  m [a]
findAllWithKVAndConditionalDBInternalArt _ ttype where' _ = getTableObject @table "FIND_ALL_WITH_KV_AND_CONDITIONAL_DB" "findAllWithKVAndConditionalDBInternal" ttype where'

updateInternalArt ::
  forall table m r.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateInternalArt meshConfig' setClause whereClause = do
  now <- getCurrentTime
  logQueryData "updateInternal" whereClause setClause [] (meshEnabled meshConfig') (modelTableName @table) (modelSchemaName @table) now

updateOneInternalArt ::
  forall table m r.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneInternalArt meshConfig' setClause whereClause = do
  now <- getCurrentTime
  logQueryData "updateOneInternal" whereClause setClause [] (meshEnabled meshConfig') (modelTableName @table) (modelSchemaName @table) now

createInternalArt ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  (a -> table Identity) ->
  a ->
  m ()
createInternalArt meshConfig' toTType a = do
  now <- getCurrentTime
  logQueryData "createInternal" [] [] [toTType a] (meshEnabled meshConfig') (modelTableName @table) (modelSchemaName @table) now

deleteInternalArt ::
  forall table m r.
  (BeamTableFlow table m r) =>
  MeshConfig ->
  Where Postgres table ->
  m ()
deleteInternalArt meshConfig' whereClause = do
  now <- getCurrentTime
  logQueryData "deleteInternal" whereClause [] [] (meshEnabled meshConfig') (modelTableName @table) (modelSchemaName @table) now

getTableInformation ::
  forall table m r.
  (BeamTableFlow table m r) =>
  Where Postgres table ->
  m (Text, Maybe Text, [[(Text, Text)]])
getTableInformation where' = pure (modelTableName @table, modelSchemaName @table, getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And where'))

getTableObject ::
  forall table m r a.
  (BeamTableFlow table m r) =>
  Text ->
  Text ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m [a]
getTableObject errorTag queryType ttype where' = do
  now <- getCurrentTime
  (tableName, schemaName, whereClause) <- getTableInformation @table where'
  artData <- ART.readAndDecodeArtData
  case artData of
    Left err -> do
      L.logError ("ART_DATA_PARSE_ERROR_OR_NOT_FOUND_" <> errorTag) $ "Art data not found for table: " <> tableName <> " schema: " <> fromMaybe "" schemaName <> " where: " <> show whereClause
      throwError $ InternalError (("ART_DATA_PARSE_ERROR_OR_NOT_FOUND_" <> errorTag) <> show err)
    Right artData' -> do
      let artDataList = map (\artdata -> (ART.queryData artdata, ART.timestamp artdata, ART.requestId artdata, ART.forkedTag artdata)) artData'
      queryData' <- ART.getArtQueryObject queryType tableName schemaName whereClause artDataList
      case queryData' of
        Nothing -> do
          logQueryData queryType where' [] [] (meshEnabled meshConfig) tableName schemaName now
          L.logWarning ("ART_QUERY_DATA_NOT_FOUND_" <> errorTag) $ "Art query data not found for table: " <> tableName <> " schema: " <> fromMaybe "" schemaName <> " where: " <> show whereClause
          pure []
        Just queryData'' -> do
          let tableObject' = ART.tableObject queryData''
              schemaName' = queryData''.schemaName
              tableName' = ART.table queryData''
          if tableName' == tableName && schemaName' == schemaName
            then do
              let tableIdentity = map decodeFromTextArt tableObject' :: [Maybe (table Identity)]
              let tableIdentity' = catMaybes tableIdentity
              logQueryData queryType where' [] tableIdentity' (meshEnabled meshConfig) tableName schemaName now
              case tableIdentity' of
                [] -> do
                  L.logDebug ("ART_TABLE_OBJECT_NOT_FOUND_" <> errorTag) $ "Art table object not found for table: " <> tableName <> " schema: " <> fromMaybe "" schemaName <> " where: " <> show whereClause <> " tableObject: " <> show tableObject'
                  pure []
                _ -> do
                  logDebug $ errorTag <> " => Found table identity for " <> tableName <> " schema: " <> fromMaybe "" schemaName <> " where: " <> show whereClause <> " tableIdentity: " <> show tableIdentity'
                  res <- mapM ttype tableIdentity'
                  pure $ catMaybes res
            else do
              L.logError ("ART_TABLE_MISMATCH_" <> errorTag) $ "Art table mismatch for table: " <> tableName <> " schema: " <> fromMaybe "" schemaName <> " where: " <> show whereClause
              throwError $ InternalError (errorTag <> " => Art data found for different table : " <> tableName' <> " schema: " <> fromMaybe "" schemaName' <> " where: actual table and schema: " <> tableName <> " || " <> fromMaybe "" schemaName <> " where: " <> show whereClause)

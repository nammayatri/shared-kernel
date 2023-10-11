module Kernel.Beam.Functions
  ( FromTType' (..),
    ToTType' (..),
    FromTType'' (..),
    ToTType'' (..),
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
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as Serialize
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (KVConnector (..), MeshConfig (..), MeshMeta)
import EulerHS.KVConnector.Utils
import qualified EulerHS.Language as L
import EulerHS.Types hiding (Log)
import qualified Kafka.Producer as KafkaProd
import Kernel.Beam.Types
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (logDebug)
import Kernel.Utils.Error (throwError)
import Sequelize (Model, ModelMeta (modelSchemaName, modelTableName), OrderBy, Set, Where)
import qualified System.Environment as SE
import System.Random
import Text.Casing

-- classes for converting from beam types to ttypes and vice versa
class
  FromTType' t a
    | t -> a
  where
  fromTType' :: MonadFlow m => t -> m (Maybe a)

class
  ToTType' t a
    | a -> t
  where
  toTType' :: a -> t

-- Below class FromTType'' and ToTType'' are only to be used with scheduler
class
  FromTType'' t a
    | a -> t
  where
  fromTType'' :: (MonadThrow m, Log m, L.MonadFlow m) => t -> m (Maybe a)

class
  ToTType'' t a
    | a -> t
  where
  toTType'' :: a -> t

meshConfig :: MeshConfig
meshConfig =
  MeshConfig
    { meshEnabled = False,
      memcacheEnabled = False,
      meshDBName = "postgres",
      ecRedisDBStream = "driver-db-sync-stream",
      kvRedis = "KVRedis",
      redisTtl = 18000,
      kvHardKilled = True,
      cerealEnabled = False
    }

runInReplica :: (L.MonadFlow m, Log m) => m a -> m a
runInReplica m = do
  L.setOptionLocal ReplicaEnabled True
  res <- m
  L.setOptionLocal ReplicaEnabled False
  pure res

setMeshConfig :: (L.MonadFlow m, HasCallStack) => Text -> Maybe Text -> MeshConfig -> m MeshConfig
setMeshConfig modelName mSchema meshConfig' = do
  schema <- maybe (L.throwException $ InternalError "Schema not found") pure mSchema
  let redisStream = if schema == "atlas_driver_offer_bpp" then "driver-db-sync-stream" else "rider-db-sync-stream"
  tables <- L.getOption KBT.Tables
  randomIntV <- L.runIO (randomRIO (1, 100) :: IO Int)
  case tables of
    Nothing -> L.throwException $ InternalError "Tables not found"
    Just tables' -> do
      let enableKVForWriteAlso = tables'.enableKVForWriteAlso
      let enableKVForRead = tables'.enableKVForRead
      if modelName `elem` (nameOfTable <$> enableKVForWriteAlso)
        then do
          if fromIntegral (percentEnable (fromJust (find (\table -> nameOfTable table == modelName) enableKVForWriteAlso))) >= randomIntV
            then do
              pure $ meshConfig' {meshEnabled = True, kvHardKilled = modelName `notElem` enableKVForRead, ecRedisDBStream = redisStream}
            else pure $ meshConfig' {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead, ecRedisDBStream = redisStream}
        else pure $ meshConfig' {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead, ecRedisDBStream = redisStream}

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

type BeamTableFlow table m =
  ( HasCallStack,
    BeamTable table,
    MonadFlow m
  )

type BeamTable table =
  ( Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity)
  )

-- findOne --

findOneWithKV ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKV where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  findOneInternal updatedMeshConfig fromTType' where'

findOneWithKVScheduler ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType'' (table Identity) a
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKVScheduler where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  findOneInternal updatedMeshConfig fromTType'' where'

findOneWithDb ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithDb = findOneInternal meshConfig fromTType'

-- findAll --

findAllWithKV ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m [a]
findAllWithKV where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  findAllInternal updatedMeshConfig fromTType' where'

findAllWithKVScheduler ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType'' (table Identity) a
  ) =>
  Where Postgres table ->
  m [a]
findAllWithKVScheduler where' = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  findAllInternal updatedMeshConfig fromTType'' where'

findAllWithDb ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  m [a]
findAllWithDb = findAllInternal meshConfig fromTType'

-- findAllWithOptions --

findAllWithOptionsKV ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKV where' orderBy mbLimit mbOffset = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  findAllWithOptionsInternal updatedMeshConfig fromTType' where' orderBy mbLimit mbOffset

findAllWithOptionsKV' ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKV' where' mbLimit mbOffset = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithOptionsKVConnector' dbConf' updatedMeshConfig where' mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsKVScheduler ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType'' (table Identity) a
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKVScheduler where' orderBy mbLimit mbOffset = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  findAllWithOptionsInternal updatedMeshConfig fromTType'' where' orderBy mbLimit mbOffset

findAllWithOptionsDb ::
  forall table m a.
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsDb = findAllWithOptionsInternal meshConfig fromTType'

-- update --

updateWithKV ::
  forall table m.
  BeamTableFlow table m =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKV setClause whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  updateInternal updatedMeshConfig setClause whereClause

updateWithKVScheduler ::
  forall table m.
  BeamTableFlow table m =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKVScheduler setClause whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  updateInternal updatedMeshConfig setClause whereClause

-- updateOne --

updateOneWithKV ::
  forall table m.
  BeamTableFlow table m =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneWithKV setClause whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  updateOneInternal updatedMeshConfig setClause whereClause

-- create --

createWithKV ::
  forall table m a.
  ( BeamTableFlow table m,
    ToTType' (table Identity) a
  ) =>
  a ->
  m ()
createWithKV a = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  createInternal updatedMeshConfig toTType' a

createWithKVScheduler ::
  forall table m a.
  ( BeamTableFlow table m,
    ToTType'' (table Identity) a
  ) =>
  a ->
  m ()
createWithKVScheduler a = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  createInternal updatedMeshConfig toTType'' a

-- delete --

deleteWithKV ::
  forall table m.
  BeamTableFlow table m =>
  Where Postgres table ->
  m ()
deleteWithKV whereClause = withUpdatedMeshConfig (Proxy @table) $ \updatedMeshConfig -> do
  deleteInternal updatedMeshConfig whereClause

deleteWithDb ::
  forall table m.
  BeamTableFlow table m =>
  Where Postgres table ->
  m ()
deleteWithDb = deleteInternal meshConfig

-- internal --

findOneInternal ::
  forall table m a.
  BeamTableFlow table m =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m (Maybe a)
findOneInternal updatedMeshConfig fromTType where' = do
  dbConf' <- getReadDBConfigInternal
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> fromTType res
    Right Nothing -> pure Nothing
    Left err -> throwError $ InternalError $ show err

findAllInternal ::
  forall table m a.
  BeamTableFlow table m =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m [a]
findAllInternal updatedMeshConfig fromTType where' = do
  dbConf' <- getReadDBConfigInternal
  result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- mapM fromTType res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsInternal ::
  forall table m a.
  BeamTableFlow table m =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsInternal updatedMeshConfig fromTType where' orderBy mbLimit mbOffset = do
  dbConf' <- getReadDBConfigInternal
  result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

getReadDBConfigInternal :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getReadDBConfigInternal = do
  inReplica <- L.getOptionLocal ReplicaEnabled
  maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica

updateInternal ::
  forall table m.
  BeamTableFlow table m =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateInternal updatedMeshConfig setClause whereClause = do
  dbConf <- getMasterDBConfig
  res <- KV.updateAllReturningWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right res' -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Updated rows KV: " <> show res'
        else do
          shouldPushToKafka <- tableInKafka (modelTableName @table)
          when shouldPushToKafka $ do
            topicName <- getKafkaTopic (modelSchemaName @table)
            mapM_ (\object' -> void $ pushToKafkaForUpdate (modelTableName @table) object' topicName (getKeyForKafka $ getLookupKeyByPKey object')) res'
          logDebug $ "Updated rows DB: " <> show res'
    Left err -> throwError $ InternalError $ show err

updateOneInternal ::
  forall table m.
  BeamTableFlow table m =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneInternal updatedMeshConfig setClause whereClause = do
  dbConf <- getMasterDBConfig
  res <- KV.updateWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right obj -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Updated row KV: " <> show obj
        else do
          shouldPushToKafka <- tableInKafka (modelTableName @table)
          when shouldPushToKafka $ do
            whenJust obj $ \object' -> do
              topicName <- getKafkaTopic (modelSchemaName @table)
              void $ pushToKafkaForUpdate (modelTableName @table) object' topicName (getKeyForKafka $ getLookupKeyByPKey object')
          logDebug $ "Updated row DB: " <> show obj
    Left err -> throwError $ InternalError $ show err

createInternal ::
  forall table m a.
  BeamTableFlow table m =>
  MeshConfig ->
  (a -> table Identity) ->
  a ->
  m ()
createInternal updatedMeshConfig toTType a = do
  let tType = toTType a
  dbConf' <- getMasterDBConfig
  result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
  case result of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Created row in KV: " <> show tType
        else do
          shouldPushToKafka <- tableInKafka (modelTableName @table)
          when shouldPushToKafka $ do
            topicName <- getKafkaTopic (modelSchemaName @table)
            void $ pushToKafkaForCreate (modelTableName @table) tType topicName (getKeyForKafka $ getLookupKeyByPKey tType)
          logDebug $ "Created row in DB: " <> show tType
    Left err -> throwError $ InternalError $ show err

deleteInternal ::
  forall table m.
  BeamTableFlow table m =>
  MeshConfig ->
  Where Postgres table ->
  m ()
deleteInternal updatedMeshConfig whereClause = do
  dbConf <- getMasterDBConfig
  res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
  case res of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Deleted rows in KV: " <> show res
        else logDebug $ "Deleted rows in DB: " <> show res
    Left err -> throwError $ InternalError $ show err

pushToKafkaForCreate :: (MonadFlow m, ToJSON (table Identity)) => Text -> table Identity -> Text -> Text -> m ()
pushToKafkaForCreate modelName messageRecord topic key = do
  isPushToKafka' <- L.runIO isPushToKafka
  when isPushToKafka' $ do
    let dbObject = getCreateObjectForKafka modelName messageRecord
    kafkaProducerTools <- L.getOption KafkaConn
    case kafkaProducerTools of
      Nothing -> throwError $ InternalError "Kafka producer tools not found"
      Just kafkaProducerTools' -> do
        mbErr <- KafkaProd.produceMessage kafkaProducerTools'.producer (kafkaMessage topic dbObject key)
        whenJust mbErr (throwError . KafkaUnableToProduceMessage)

pushToKafkaForUpdate :: (MonadFlow m, ToJSON (table Identity)) => Text -> table Identity -> Text -> Text -> m ()
pushToKafkaForUpdate modelName setClause topic key = do
  isPushToKafka' <- L.runIO isPushToKafka
  when isPushToKafka' $ do
    let dbObject = getUpdateObjectForKafka modelName setClause
    kafkaProducerTools <- L.getOption KafkaConn
    case kafkaProducerTools of
      Nothing -> throwError $ InternalError "Kafka producer tools not found"
      Just kafkaProducerTools' -> do
        mbErr <- KafkaProd.produceMessage kafkaProducerTools'.producer (kafkaMessage topic dbObject key)
        whenJust mbErr (throwError . KafkaUnableToProduceMessage)

kafkaMessage :: ToJSON a => Text -> a -> Text -> KafkaProd.ProducerRecord
kafkaMessage topicName event key =
  KafkaProd.ProducerRecord
    { prTopic = KafkaProd.TopicName topicName,
      prPartition = KafkaProd.UnassignedPartition,
      prKey = Just $ TE.encodeUtf8 key,
      prValue = Just . LBS.toStrict $ encode event
    }

getKafkaTopic :: (MonadFlow m) => Maybe Text -> m Text
getKafkaTopic mSchema = do
  modelName <- maybe (L.throwException $ InternalError "Schema not found") pure mSchema
  if modelName == "atlas_driver_offer_bpp" then pure "driver-drainer" else pure "rider-drainer"

getCreateObjectForKafka :: ToJSON a => Text -> a -> Value
getCreateObjectForKafka modelName dbObject =
  object
    [ "contents" .= dbObject,
      "tag" .= ((T.pack . pascal . T.unpack) modelName <> "Object")
    ]

getUpdateObjectForKafka :: ToJSON (table Identity) => Text -> table Identity -> Value
getUpdateObjectForKafka modelName object' =
  object
    [ "contents"
        .= toJSON object',
      "tag" .= T.pack (pascal (T.unpack modelName) <> "Object"),
      "type" .= ("UPDATE" :: Text)
    ]

isPushToKafka :: IO Bool
isPushToKafka = fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv "PUSH_TO_KAFKA"

getKeyForKafka :: Text -> Text
getKeyForKafka pKeyText = do
  let shard = getShardedHashTag pKeyText
  pKeyText <> shard

tableInKafka :: L.MonadFlow m => Text -> m Bool
tableInKafka modelName = do
  tables <- L.getOption KBT.Tables
  case tables of
    Nothing -> pure False
    Just tables' -> pure $ modelName `elem` (tables'.kafkaNonKVTables)

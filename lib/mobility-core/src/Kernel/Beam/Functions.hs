module Kernel.Beam.Functions where

import qualified Data.Serialize as Serialize
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import EulerHS.CachedSqlDBQuery (SqlReturning)
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (KVConnector (..), MeshConfig (..), MeshMeta)
import qualified EulerHS.Language as L
import EulerHS.Types hiding (Log)
import Kernel.Beam.Types
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (logDebug)
import Kernel.Utils.Error (throwError)
import Sequelize (Model, ModelMeta (modelSchemaName, modelTableName), OrderBy, Set, Where)
import System.Random

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

type FromBeamTableFlow table m a =
  ( BeamTableFlow table m,
    FromTType' (table Identity) a
  )

type ToBeamTableFlow table m a =
  ( BeamTableFlow table m,
    ToTType' (table Identity) a
  )

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

findOneWithKV ::
  forall table m a.
  FromBeamTableFlow table m a =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKV where' = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> fromTType' res
    Right Nothing -> pure Nothing
    Left err -> throwError $ InternalError $ show err

findOneWithKVScheduler ::
  forall table m a.
  ( HasCallStack,
    FromTType'' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKVScheduler where' = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> fromTType'' res
    Right Nothing -> pure Nothing
    Left err -> throwError $ InternalError $ show err

findAllWithKV ::
  forall table m a.
  FromBeamTableFlow table m a =>
  Where Postgres table ->
  m [a]
findAllWithKV where' = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithKVScheduler ::
  forall table m a.
  ( HasCallStack,
    FromTType'' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  Where Postgres table ->
  m [a]
findAllWithKVScheduler where' = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- mapM fromTType'' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsKV ::
  forall table m a.
  FromBeamTableFlow table m a =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKV where' orderBy mbLimit mbOffset = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsKVScheduler ::
  forall table m a.
  ( HasCallStack,
    FromTType'' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKVScheduler where' orderBy mbLimit mbOffset = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType'' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findOneWithDb ::
  forall table m a.
  FromBeamTableFlow table m a =>
  Where Postgres table ->
  m (Maybe a)
findOneWithDb where' = do
  let updatedMeshConfig = meshConfig {meshEnabled = False, kvHardKilled = True}
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> fromTType' res
    Right Nothing -> pure Nothing
    Left err -> throwError $ InternalError $ show err

findAllWithDb ::
  forall table m a.
  FromBeamTableFlow table m a =>
  Where Postgres table ->
  m [a]
findAllWithDb where' = do
  let updatedMeshConfig = meshConfig {meshEnabled = False, kvHardKilled = True}
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsDb ::
  forall table m a.
  FromBeamTableFlow table m a =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsDb where' orderBy mbLimit mbOffset = do
  let updatedMeshConfig = meshConfig {meshEnabled = False, kvHardKilled = True}
  inReplica <- L.getOptionLocal ReplicaEnabled
  dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
  result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

updateWithKV ::
  forall table m.
  BeamTableFlow table m =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKV setClause whereClause = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.updateAllWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right res' -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Updated rows KV: " <> show res'
        else logDebug $ "Updated rows DB: " <> show res'
      pure ()
    Left err -> throwError $ InternalError $ show err

updateWithKVScheduler ::
  forall table m.
  ( HasCallStack,
    BeamRuntime Postgres Pg,
    SqlReturning Pg Postgres,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKVScheduler setClause whereClause = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.updateAllWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right res' -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Updated rows KV: " <> show res'
        else logDebug $ "Updated rows DB: " <> show res'
      pure ()
    Left err -> throwError $ InternalError $ show err

updateOneWithKV ::
  forall table m.
  BeamTableFlow table m =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneWithKV setClause whereClause = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.updateWoReturningWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

createWithKV ::
  forall table m a.
  ToBeamTableFlow table m a =>
  a ->
  m ()
createWithKV a = do
  let tType = toTType' a
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  dbConf' <- getMasterDBConfig
  result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
  case result of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Created row in KV: " <> show tType
        else logDebug $ "Created row in DB: " <> show tType
      pure ()
    Left err -> throwError $ InternalError $ show err

createWithKVScheduler ::
  forall table m a.
  ( HasCallStack,
    ToTType'' (table Identity) a,
    SqlReturning Pg Postgres,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  a ->
  m ()
createWithKVScheduler a = do
  let tType = toTType'' a
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  dbConf' <- getMasterDBConfig
  result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
  case result of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Created row in KV: " <> show tType
        else logDebug $ "Created row in DB: " <> show tType
      pure ()
    Left err -> throwError $ InternalError $ show err

deleteWithKV ::
  forall table m.
  BeamTableFlow table m =>
  Where Postgres table ->
  m ()
deleteWithKV whereClause = do
  updatedMeshConfig <- setMeshConfig (modelTableName @table) (modelSchemaName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
  case res of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Deleted rows in KV: " <> show res
        else logDebug $ "Deleted rows in DB: " <> show res
      pure ()
    Left err -> throwError $ InternalError $ show err

deleteWithDb ::
  forall table m.
  BeamTableFlow table m =>
  Where Postgres table ->
  m ()
deleteWithDb whereClause = do
  let updatedMeshConfig = meshConfig {meshEnabled = False, kvHardKilled = True}
  dbConf <- getMasterDBConfig
  res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
  case res of
    Right _ -> do
      if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
        then logDebug $ "Deleted rows in KV: " <> show res
        else logDebug $ "Deleted rows in DB: " <> show res
      pure ()
    Left err -> throwError $ InternalError $ show err

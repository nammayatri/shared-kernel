module Kernel.Beam.Connection.EnvVars where

import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)

postgresConnectionName :: IO Text
postgresConnectionName = T.pack . fromMaybe "postgres" <$> lookupEnv "POSTGRES_CONNECTION_NAME"

postgresR1ConnectionName :: IO Text
postgresR1ConnectionName = T.pack . fromMaybe "postgresR1" <$> lookupEnv "POSTGRES_R1_CONNECTION_NAME"

getPostgresPoolStripes :: IO Int
getPostgresPoolStripes = fromMaybe 1 . (readMaybe =<<) <$> lookupEnv "POSTGRES_POOL_STRIPES"

getPostgresPoolIdleTime :: IO Integer
getPostgresPoolIdleTime = fromMaybe 20000 . (readMaybe =<<) <$> lookupEnv "POSTGRES_POOL_IDLE_TIME"

getPostgresPoolMax :: IO Int
getPostgresPoolMax = fromMaybe 5 . (readMaybe =<<) <$> lookupEnv "POSTGRES_POOL_MAX"

postgresLocationDBConnectionName :: IO Text
postgresLocationDBConnectionName = T.pack . fromMaybe "postgresLocationDB" <$> lookupEnv "POSTGRES_LOCATION_DB_CONNECTION_NAME"

postgresLocationDBReplicaConnectionName :: IO Text
postgresLocationDBReplicaConnectionName = T.pack . fromMaybe "postgresLocationDBReplica" <$> lookupEnv "POSTGRES_LOCATION_DB_REPLICA_CONNECTION_NAME"

toTitle :: String -> String
toTitle "" = ""
toTitle (x : xs) = toUpper x : map toLower xs

getPreparePosgreSqlConnection :: IO Bool
getPreparePosgreSqlConnection = do
  envVal <- lookupEnv "PREPARE_POSGRESQL_CONNECTION"
  pure (Just False /= (readMaybe . toTitle =<< envVal))

getPreparePosgreSqlR1Connection :: IO Bool
getPreparePosgreSqlR1Connection = do
  envVal <- lookupEnv "PREPARE_POSGRESQL_R1_CONNECTION"
  pure (Just False /= (readMaybe . toTitle =<< envVal))

getPrepareRedisClusterConnection :: IO Bool
getPrepareRedisClusterConnection = do
  envVal <- lookupEnv "PREPARE_REDIS_CLUSTER_CONNECTION"
  pure (Just False /= (readMaybe . toTitle =<< envVal))

getPrepareLocationDBConnection :: IO Bool
getPrepareLocationDBConnection = do
  envVal <- lookupEnv "PREPARE_LOCATION_DB_CONNECTION"
  pure (Just False /= (readMaybe . toTitle =<< envVal))

getPrepareLocationDBReplicaConnection :: IO Bool
getPrepareLocationDBReplicaConnection = do
  envVal <- lookupEnv "PREPARE_LOCATION_DB_REPLICA_CONNECTION"
  pure (Just False /= (readMaybe . toTitle =<< envVal))

-- Secondary Cloud Redis connection
getPrepareSecondaryRedisClusterConnection :: IO Bool
getPrepareSecondaryRedisClusterConnection = do
  envVal <- lookupEnv "PREPARE_SECONDARY_REDIS_CLUSTER_CONNECTION"
  pure (Just False /= (readMaybe . toTitle =<< envVal))

getRunInMasterCloudRedisCell :: IO Bool
getRunInMasterCloudRedisCell = do
  envVal <- lookupEnv "RUN_IN_MASTER_REDIS_CELL"
  pure (fromMaybe False (readMaybe =<< envVal))

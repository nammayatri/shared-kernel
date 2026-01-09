{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Kernel.Beam.Types where

import Data.Time.Clock
import qualified Database.Beam.Postgres as BP
import EulerHS.Prelude
import EulerHS.Types (DBConfig, OptionEntity)
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Types.Common as KTC

data LogLevelLastUpdatedTime = LogLevelLastUpdatedTime
  deriving stock (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity LogLevelLastUpdatedTime UTCTime

data DynamicLogLevelConfig = DynamicLogLevelConfig
  deriving stock (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity DynamicLogLevelConfig (HashMap Text KTC.LogLevel)

data PsqlDbCfg = PsqlDbCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PsqlDbCfg (DBConfig BP.Pg)

data Tables = Tables
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity Tables KTC.Tables

data KvConfigLastUpdatedTime = KvConfigLastUpdatedTime
  deriving stock (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity KvConfigLastUpdatedTime UTCTime

data KvConfigUpdateFrequency = KvConfigUpdateFrequency
  deriving stock (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity KvConfigUpdateFrequency Int

data PsqlDbCfgR1 = PsqlDbCfgR1
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PsqlDbCfgR1 (DBConfig BP.Pg)

newtype DatabaseError = DatabaseError
  { errorMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance Exception DatabaseError

type BeamFlow = ReaderT BeamState

data BeamState = BeamState
  { dbConf :: DBConfig BP.Pg
  }
  deriving (Eq, Show, Generic)

data PsqlLocDbCfg = PsqlLocDbCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PsqlLocDbCfg (DBConfig BP.Pg)

data PsqlLocReplicaDbCfg = PsqlLocReplicaDbCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PsqlLocReplicaDbCfg (DBConfig BP.Pg)

data ReplicaEnabled = ReplicaEnabled
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity ReplicaEnabled Bool

data MasterReadEnabled = MasterReadEnabled
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity MasterReadEnabled Bool

data KafkaConn = KafkaConn
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity KafkaConn KafkaProducerTools

-- Flag to enable multi-cloud Redis reads for a query
data MultiCloudEnabled = MultiCloudEnabled
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity MultiCloudEnabled Bool

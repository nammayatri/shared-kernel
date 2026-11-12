module Kernel.Beam.Connection.Types where

import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis.Config

data ConnectionConfigDriver = ConnectionConfigDriver
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisClusterCfgs :: [HedisCloudCfg]
  }

data ConnectionConfigRider = ConnectionConfigRider
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisClusterCfgs :: [HedisCloudCfg]
  }

data ConnectionConfigDashboard = ConnectionConfigDashboard
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisClusterCfgs :: [HedisCloudCfg]
  }

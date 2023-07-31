module Kernel.Beam.Connection.Types where

import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis.Config

data ConnectionConfigDriver = ConnectionConfigDriver
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisClusterCfg :: HedisCfg,
    locationDbCfg :: EsqDBConfig,
    locationDbReplicaCfg :: EsqDBConfig
  }

data ConnectionConfigRider = ConnectionConfigRider
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisClusterCfg :: HedisCfg
  }

{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Queries.SystemConfigs where

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), findOneWithDb)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.SystemConfigs as Domain
import Sequelize as Se

findById :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => Text -> m (Maybe Text)
findById cfgId = do
  findOneWithDb [Se.Is BeamSC.id $ Se.Eq cfgId] <&> (<&> Domain.configValue)
    >>= maybe (incrementKvConfigFailedCounter ("kv_config_find_failed_" <> schemaName (Proxy :: Proxy BeamSC.SystemConfigsT)) >> pure Nothing) (pure . Just)

instance FromTType' BeamSC.SystemConfigs Domain.SystemConfigs where
  fromTType' BeamSC.SystemConfigsT {..} = do
    pure $ Just Domain.SystemConfigs {..}

instance ToTType' BeamSC.SystemConfigs Domain.SystemConfigs where
  toTType' Domain.SystemConfigs {..} = do
    BeamSC.SystemConfigsT {..}

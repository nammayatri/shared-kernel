{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Queries.SystemConfigs where

import qualified EulerHS.Language as L
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), findOneWithDb)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Error
import Kernel.Types.SystemConfigs as Domain
import Sequelize as Se

findById :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => Text -> m (Maybe Text)
findById cfgId = do
  findOneWithDb [Se.Is BeamSC.id $ Se.Eq cfgId] <&> (<&> Domain.configValue)
    >>= maybe (incrementSystemConfigsFailedCounter ("system_configs_find_failed_" <> schemaName (Proxy :: Proxy BeamSC.SystemConfigsT) <> "_" <> cfgId) >> pure Nothing) (pure . Just)

findById' :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => Text -> m (Domain.SystemConfigs)
findById' cfgId = do
  config <- findOneWithDb [Se.Is BeamSC.id $ Se.Eq cfgId]
  maybe (L.throwException $ InternalError "Not able to find CAC config by id") pure config

instance FromTType' BeamSC.SystemConfigs Domain.SystemConfigs where
  fromTType' BeamSC.SystemConfigsT {..} = do
    pure $ Just Domain.SystemConfigs {..}

instance ToTType' BeamSC.SystemConfigs Domain.SystemConfigs where
  toTType' Domain.SystemConfigs {..} = do
    BeamSC.SystemConfigsT {..}

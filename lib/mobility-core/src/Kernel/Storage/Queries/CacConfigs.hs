{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Queries.CacConfigs where

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), findOneWithDb)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Storage.Beam.CacConfigs as BeamSC
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.CacConfigs as Domain
import Kernel.Types.CacheFlow (CacheFlow)
import Sequelize as Se

findById :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.CacConfigsT) => Text -> m (Maybe Text)
findById cfgId = do
  findOneWithDb [Se.Is BeamSC.id $ Se.Eq cfgId] <&> (<&> Domain.configValue)
    >>= maybe (incrementCacConfigsFailedCounter ("cac_configs_find_failed_" <> schemaName (Proxy :: Proxy BeamSC.CacConfigsT) <> "_" <> cfgId) >> pure Nothing) (pure . Just)

instance FromTType' BeamSC.CacConfigs Domain.CacConfigs where
  fromTType' BeamSC.CacConfigsT {..} = do
    pure $ Just Domain.CacConfigs {..}

instance ToTType' BeamSC.CacConfigs Domain.CacConfigs where
  toTType' Domain.CacConfigs {..} = do
    BeamSC.CacConfigsT {..}

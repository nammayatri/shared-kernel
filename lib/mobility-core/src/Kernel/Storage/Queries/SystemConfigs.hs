{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Queries.SystemConfigs where

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), findOneWithKV)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Error
import Kernel.Types.SystemConfigs as Domain
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Sequelize as Se

findById :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName BeamSC.SystemConfigsT) => Text -> m Text
findById cfgId = (findOneWithKV [Se.Is BeamSC.id $ Se.Eq cfgId] <&> (<&> Domain.configValue)) >>= fromMaybeM (InternalError "No entry in table")

instance FromTType' BeamSC.SystemConfigs Domain.SystemConfigs where
  fromTType' BeamSC.SystemConfigsT {..} = do
    pure $ Just Domain.SystemConfigs {..}

instance ToTType' BeamSC.SystemConfigs Domain.SystemConfigs where
  toTType' Domain.SystemConfigs {..} = do
    BeamSC.SystemConfigsT {..}

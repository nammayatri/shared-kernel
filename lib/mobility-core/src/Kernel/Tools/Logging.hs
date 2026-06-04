{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Kernel.Tools.Logging where

import qualified Data.HashMap.Internal as HM
import Data.Time hiding (getCurrentTime)
import qualified EulerHS.Language as L
import Kernel.Beam.Lib.UtilsTH (HasSchemaName, schemaName)
import qualified Kernel.Beam.Types as KT
import Kernel.Prelude
import Kernel.Randomizer (getRandomInRange)
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Storage.Esqueleto.Config (HasEsqEnv)
import Kernel.Storage.Hedis.Config
import qualified Kernel.Storage.Queries.SystemConfigs as QSC
import Kernel.Tools.Metrics.CoreMetrics (HasCoreMetrics, incrementSystemConfigsFailedCounter)
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow (HasCacConfig, HasCacheConfig, HasInMemEnv)
import Kernel.Types.Logging
import Kernel.Types.Time
import Kernel.Utils.IOLogging (HasLog, updateLogLevelAndRawSql)
import Kernel.Utils.Text

withDynamicLogLevel ::
  (HasLog f, HasCoreMetrics f, HasEsqEnv m f, HedisFlow m f, HasInMemEnv f, HasCacheConfig f, HasSchemaName BeamSC.SystemConfigsT, MonadReader f m, MonadFlow m, HasCacConfig f) =>
  Text ->
  m a ->
  m a
withDynamicLogLevel keyName fn = do
  mbDynamicLogLevelConfig <- getDynamicLogLevelConfig
  mbLogLevel <- resolveDynamicLogLevel (HM.lookup keyName =<< mbDynamicLogLevelConfig)
  local (modifyEnv mbLogLevel) fn
  where
    modifyEnv mbLogLevel env = do
      let logEnv = env.loggerEnv
          updLogEnv = updateLogLevelAndRawSql mbLogLevel logEnv
      env{loggerEnv = updLogEnv}

-- | Resolve a matched 'DynamicLogLevel' config into the effective 'LogLevel' to apply.
--
-- When the config carries a stagger percentage, the level is applied only on a
-- random toss so that roughly @staggerPercentage@% of calls get it. Without a
-- stagger the level always applies — keeping the previous (bare-level) behaviour.
resolveDynamicLogLevel :: MonadIO m => Maybe DynamicLogLevel -> m (Maybe LogLevel)
resolveDynamicLogLevel Nothing = pure Nothing
resolveDynamicLogLevel (Just dynamicLogLevel) = case dynamicLogLevel.staggerPercentage of
  Nothing -> pure $ Just dynamicLogLevel.logLevel
  Just pct -> do
    toss <- getRandomInRange (1, 100 :: Int)
    pure $ if toss <= pct then Just dynamicLogLevel.logLevel else Nothing

getDynamicLogLevelConfig ::
  (HasLog f, HasCoreMetrics f, HasEsqEnv m f, HedisFlow m f, HasInMemEnv f, HasCacheConfig f, HasSchemaName BeamSC.SystemConfigsT, MonadReader f m, MonadFlow m, HasCacConfig f) => m (Maybe (HM.HashMap Text DynamicLogLevel))
getDynamicLogLevelConfig = do
  now <- getCurrentTime
  shouldFetchFromDB <-
    (isNothing <$>) . runMaybeT $ do
      kvConfigLastUpdatedTime <- MaybeT $ L.getOption KT.LogLevelLastUpdatedTime
      kvConfigUpdateFrequency <- MaybeT $ L.getOption KT.KvConfigUpdateFrequency
      MaybeT . pure $ if round (diffUTCTime now kvConfigLastUpdatedTime) > kvConfigUpdateFrequency then Nothing else Just False
  if shouldFetchFromDB
    then do
      res <- QSC.findById "log_levels" >>= pure . decodeFromText' @(HM.HashMap Text DynamicLogLevel)
      maybe (incrementSystemConfigsFailedCounter ("system_configs_decode_failed_" <> schemaName (Proxy :: Proxy BeamSC.SystemConfigsT) <> "_log_levels")) (L.setOption KT.DynamicLogLevelConfig) res
      void $ L.setOption KT.LogLevelLastUpdatedTime now
      pure res
    else L.getOption KT.DynamicLogLevelConfig

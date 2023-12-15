{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}

module Kernel.Tools.Metrics.KvConfigMetrics where

import qualified Debug.Trace as T
import Euler.Events.MetricApi.MetricApi
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (OptionEntity)

data KvConfigMetrics
  = KvConfigDecodeFailure
  | KvConfigFindFailed
  deriving stock (Generic, Show, Eq)

data KvConfigsCounterHandler = KvConfigsCounterHandler
  { pubKvConfigMetric :: KvConfigMetrics -> IO ()
  }
  deriving (Generic)

-- using for set option entity
data KvConfigsCounterHandler' = KvConfigsCounterHandler'
  deriving stock (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity KvConfigsCounterHandler' KvConfigsCounterHandler

kv_config_decode_failure :: PromRep 'Counter "kv_config_decode_failure" '[]
kv_config_decode_failure = counter #kv_config_decode_failure .& build

kv_config_find_failed :: PromRep 'Counter "kv_config_find_failed" '[]
kv_config_find_failed = counter #kv_config_find_failed .& build

makeKvConfigMetrics :: IO KvConfigsCounterHandler
makeKvConfigMetrics = do
  metrics <- register collectionKvConfigMetrics
  pure $
    KvConfigsCounterHandler $ \case
      KvConfigDecodeFailure -> inc (metrics </> #kv_config_decode_failure)
      KvConfigFindFailed -> inc (metrics </> #kv_config_find_failed)
  where
    collectionKvConfigMetrics =
      kv_config_decode_failure
        .> kv_config_find_failed
        .> MNil

publishKvConfigMetric :: L.MonadFlow m => KvConfigMetrics -> m ()
publishKvConfigMetric metric = do
  maybeMetricsHandler <- L.getOption KvConfigsCounterHandler'
  metricsHandler <- case maybeMetricsHandler of
    Nothing -> do
      newMetricsHandler <- L.runIO makeKvConfigMetrics
      L.setOption KvConfigsCounterHandler' newMetricsHandler
      T.trace "inside Nothing of metric Handles" pure newMetricsHandler
    Just existingMetricsHandler -> pure existingMetricsHandler
  T.trace "here at push metrics " $ L.runIO $ pubKvConfigMetric metricsHandler metric

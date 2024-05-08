module Kernel.Types.RCFlow where

import Kernel.External.Encryption (EncFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App (MonadFlow)
import Kernel.Utils.Common (CacheFlow)

type RCFlow m r =
  ( MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  )

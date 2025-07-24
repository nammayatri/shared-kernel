module Kernel.External.ConversionEvent.Interface where

import Data.Monoid
import Kernel.External.ConversionEvent.Interface.Meta
import Kernel.External.ConversionEvent.Interface.Types
import Kernel.External.ConversionEvent.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common as Common
import Prelude

conversionEvent :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => ConversionEventServiceConfig -> ConversionReqType -> m ()
conversionEvent serviceConfig req = do
  logDebug $ "request come for logEvent"
  case serviceConfig of
    MetaConfig cfg -> do
      logDebug $ "req received" <> Common.show cfg
      conversionEventApiCall cfg req
